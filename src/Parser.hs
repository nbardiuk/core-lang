module Parser where

import Control.Arrow (first)
import Data.Char (isAlpha, isDigit)

-- | A token is never empty
type Token = String

type Parser a = [Token] -> [(a, [Token])]

clex :: String -> [Token]
clex (c:cs)
  | isWhiteSpace c = clex cs
clex (c:cs)
  | isDigit c = num_token : clex rest_cs
  where
    num_token = c : takeWhile isDigit cs
    rest_cs = dropWhile isDigit cs
clex (c:cs)
  | isAlpha c = var_token : clex rest_cs
  where
    var_token = c : takeWhile isIdChar cs
    rest_cs = dropWhile isIdChar cs
clex ('-':'-':cs) = clex $ dropWhile (/= '\n') cs
clex (a:b:cs)
  | [a, b] `elem` ["<=", "==", "/=", ">=", "->"] = [a, b] : clex cs
clex (c:cs) = [c] : clex cs
clex [] = []

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')

isWhiteSpace :: Char -> Bool
isWhiteSpace = (`elem` " \t\n")

pLit :: String -> Parser String
pLit s = pSat (== s)

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

pVar :: Parser String
pVar = pSat (\t -> (t `notElem` keywords) && (isAlpha . head) t)

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 ts = p1 ts ++ p2 ts

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 ts =
  [(combine v1 v2, ts2) | (v1, ts1) <- p1 ts, (v2, ts2) <- p2 ts1]

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 ts =
  [ (combine v1 v2 v3, ts3)
  | (v1, ts1) <- p1 ts
  , (v2, ts2) <- p2 ts1
  , (v3, ts3) <- p3 ts2
  ]

pThen4 ::
     (a -> b -> c -> d -> e)
  -> Parser a
  -> Parser b
  -> Parser c
  -> Parser d
  -> Parser e
pThen4 combine p1 p2 p3 p4 ts =
  [ (combine v1 v2 v3 v4, ts4)
  | (v1, ts1) <- p1 ts
  , (v2, ts2) <- p2 ts1
  , (v3, ts3) <- p3 ts2
  , (v4, ts4) <- p4 ts3
  ]

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pAlt` pEmpty []

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep pa pb = pThen (:) pa (pZeroOrMore (pThen seq pb pa))

pEmpty :: a -> Parser a
pEmpty a ts = [(a, ts)]

pApply :: Parser a -> (a -> b) -> Parser b
pApply p f = (first f <$>) . p

pSat :: (String -> Bool) -> Parser String
pSat p (t:ts)
  | p t = [(t, ts)]
pSat _ _ = []
