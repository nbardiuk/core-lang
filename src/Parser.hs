module Parser where

import Data.Char (isAlpha, isDigit, isSpace)

-- | A token is never empty
type Token = String

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
