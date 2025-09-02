module Tokenizer (tokenize, splitUntilNotNumber, splitFirstExceptEscape) where

import Types (JSONToken(..))
import Data.Char (isDigit, isSpace)

tokenize :: String -> [JSONToken]
tokenize ('{':xs) = JSONTokenOpenBrace : tokenize xs
tokenize ('}':xs) = JSONTokenCloseBrace : tokenize xs
tokenize ('[':xs) = JSONTokenOpenBracket : tokenize xs
tokenize (']':xs) = JSONTokenCloseBracket : tokenize xs
tokenize (':':xs) = JSONTokenColon : tokenize xs
tokenize (',':xs) = JSONTokenComma : tokenize xs
tokenize ('"':xs) = let (before, after) = splitFirstExceptEscape '"' xs
                         in JSONTokenText before:tokenize after
tokenize ('n':'u':'l':'l':xs) = JSONTokenNull : tokenize xs
tokenize ('t':'r':'u':'e':xs) = JSONTokenBoolean True : tokenize xs
tokenize ('f':'a':'l':'s':'e':xs) = JSONTokenBoolean False : tokenize xs
tokenize (x:xs)
  | isSpace x = tokenize xs
  -- 数字トークンは数字かマイナスで始まる。ピリオド始まりはありえない。
  | isDigit x || x == '-' = let (before, after) = splitUntilNotNumber xs
                 in JSONTokenNumber (read (x:before) :: Double) : tokenize after
  | otherwise = error ("Unexpected character: " ++ [x])
tokenize [] = []

splitUntilNotNumber :: String -> (String, String)
splitUntilNotNumber = go
  where
    go "" = ("", "")
    go (c:cs)
      | isDigit c || c == '.' = let (before, after) = go cs
                                 in (c:before, after)
      | otherwise = ("", c:cs)

splitFirstExceptEscape :: Char -> String -> (String, String)
splitFirstExceptEscape ch = go
  where
    go "" = ("", "")
    go (c:cs)
      | c == ch  = ("", cs)
      | c == '\\' = let (before, after) = go (tail cs)
                     in (head cs:before, after)
      | otherwise = let (before, after) = go cs
                     in (c:before, after)
