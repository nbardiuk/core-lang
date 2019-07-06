module Language ( CoreProgram
                , CoreScDefn
                , CoreExpr
                , Name
                , CoreAlt
                , Alter
                , Expr (..)
                , isAtomicExpr
                ) where

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
