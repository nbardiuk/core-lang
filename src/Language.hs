module Language where

import           Data.List (intersperse)


data Expr a = EVar Name             -- ^ Variables
            | ENum Int              -- ^ Numbers
            | EConstr Int Int       -- ^ Constructor tag arity
            | EAp (Expr a) (Expr a) -- ^ Applications
            | ELet                  -- ^ Let(rec) expressions
                 IsRec              -- ^   True = recursive
                 [(a, Expr a)]      -- ^   Definitions
                 (Expr a)           -- ^   Body of let(rec)
            | ECase                 -- ^ Case expression
                 (Expr a)           -- ^   Expression to scrutinies
                 [Alter a]          -- ^   Alternatives
            | ELam [a] (Expr a)     -- ^ Lambda abstractions
            deriving (Show)

type Alter a = (Int       -- ^ tag
              , [a]       -- ^ bound variables
              , Expr a    -- ^ expression right to the arrow
               )

type Name = String

type CoreExpr = Expr Name
type CoreAlt = Alter Name

type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive = True
nonRecursive = False

bindersOf :: [(a,b)] -> [a]
bindersOf defns = [name | (name, rhs) <- defns]

rhssOf :: [(a,b)] -> [b]
rhssOf defns = [rhs | (name, rhs) <- defns]

-- | expression with no internal structure
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum v) = True
isAtomicExpr _        = False

type Program a = [ScDefn a]
type CoreProgram = Program Name

-- | supercombinator definition
type ScDefn a = ( Name    -- ^ name
                , [a]     -- ^ arguments
                , Expr a  -- ^ definition
                )
type CoreScDefn = ScDefn Name

-- | standard prelude for Core
preludeDefs :: CoreProgram
preludeDefs = [ ("I", ["x"], EVar "x")
              , ("K", ["x", "y"], EVar "x")
              , ("K1", ["x", "y"], EVar "y")
              , ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x"))
                                           (EAp (EVar "g") (EVar "x")))
              , ("compose", ["f", "g", "x"], EAp (EVar "f")
                                                 (EAp (EVar "g") (EVar "x")))
              , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
              ]

-- TODO extract pretty printer module
-- TODO add unit tests for pretty printer

data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline

iNil :: Iseq
iNil = INil

iNum :: Int -> Iseq
iNum n = IStr (show n)

iStr :: String -> Iseq
iStr = IStr

iAppend :: Iseq -> Iseq -> Iseq
iAppend INil s  = s
iAppend  s INil = s
iAppend s1 s2   = IAppend s1 s2

iNewline :: Iseq
iNewline = INewline

iIndent :: Iseq -> Iseq
iIndent = IIndent

iDisplay :: Iseq -> String
iDisplay seq = flatten 0 [(seq, 0)]

flatten :: Int            -- Current column; 0 for first column
        -> [(Iseq, Int)]  -- Work list
        -> String         -- Result
flatten _ [] = ""
flatten col ((INil, _) : seqs) = flatten col seqs
flatten col ((IStr s, _) : seqs) = s ++ flatten (col + length s) seqs
flatten col ((IAppend s1 s2, indent) : seqs) = flatten col ((s1, indent) : (s2, indent): seqs)
flatten _ ((INewline, indent) : seqs) = '\n' : (space indent) ++ (flatten indent seqs)
flatten col ((IIndent seq, _) : seqs) = flatten col ((seq, col) : seqs)

space :: Int -> String
space n = replicate n ' '

iConcat :: [Iseq] -> Iseq
iConcat = foldr iAppend iNil

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave sep is = iConcat (intersperse sep is)

pprExpr :: CoreExpr -> Iseq
pprExpr (EVar v) = iStr v
pprExpr (ENum n) = iStr (show n)
pprExpr (EAp e1 e2) = iConcat [pprExpr e1, iStr " ", pprAExpr e2]
pprExpr (ELet isrec defns expr) = iConcat [ iStr keyword, iNewline
                                          , iStr " ", iIndent (pprDefns defns), iNewline
                                          , iStr "in ", pprExpr expr ]
                                            where keyword = if isrec then "let" else "letrec"
pprExpr (ECase e alts) = iConcat [ iStr "case ", pprExpr e, iStr " of", iNewline
                                 , iIndent (iInterleave iNewline (map pprAlter alts))]
pprExpr (ELam vs e) = iConcat [ iStr "(\\ "
                              , iInterleave (iStr " ") (map iStr vs)
                              , iStr " -> ", pprExpr e
                              , iStr ")"]

pprAlter :: CoreAlt -> Iseq
pprAlter (tag, vs, e) = iConcat [ iStr "<", iNum tag, iStr "> "
                                , iInterleave (iStr " ") (map iStr vs)
                                , iStr " -> "
                                , pprExpr e]

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where sep = iConcat [iStr ";", iNewline]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) = iConcat [iStr name, iStr " = ", iIndent (pprExpr expr)]

pprAExpr :: CoreExpr -> Iseq
pprAExpr e | isAtomicExpr e = pprExpr e
           | otherwise      = iConcat [iStr "(",  pprExpr e, iStr ")"]

pprProgram :: CoreProgram -> Iseq
pprProgram defns = iInterleave (iConcat [iNewline, iNewline]) (map pprScDefn defns)

pprScDefn :: CoreScDefn -> Iseq
pprScDefn (name, args, expr) = iConcat [iStr name, iStr " "
                                     , iInterleave (iStr " ") (map iStr args)
                                     , iStr " = ", iIndent (pprExpr expr)]

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)
