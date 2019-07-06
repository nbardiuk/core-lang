module PrettyPrinter (pprint) where

import           Data.List (intersperse)
import           Language  (Alter, CoreAlt, CoreExpr, CoreProgram, CoreScDefn,
                            Expr (..), Name, isAtomicExpr)

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
