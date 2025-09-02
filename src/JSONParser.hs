module JSONParser (parse, JSONValue(..), tokenize, splitFirstExceptEscape, splitUntilNotNumber, JSONToken(..)) where
import Data.Char (isDigit, isSpace)

data JSONValue
  = JSONString String
  | JSONNumber Double
  | JSONBool Bool
  | JSONNull
  | JSONArray [JSONValue]
  | JSONObject [(String, JSONValue)]
  deriving (Show, Eq)

parse :: String -> JSONValue
parse input = let tokens = tokenize input
                  (result, rest) = parseTokens tokens
               in case rest of
                  [] -> result
                  _ -> error "Unexpected token"

parseTokens :: [JSONToken] -> (JSONValue, [JSONToken])
parseTokens (JSONTokenOpenBrace:tx) = parseObject tx
parseTokens (JSONTokenText t:tx) = (JSONString t, tx)
parseTokens (JSONTokenNumber t:tx) = (JSONNumber t, tx)
parseTokens (JSONTokenBoolean t:tx) = (JSONBool t, tx)
parseTokens (JSONTokenOpenBracket:tx) = parseArray tx
parseTokens (t:_) = error ("Unexpected token " ++ show t)
parseTokens [] = (JSONNull, [])

parseObject :: [JSONToken] -> (JSONValue, [JSONToken])
parseObject tx = go tx []
  where
    go :: [JSONToken] -> [(String, JSONValue)] -> (JSONValue, [JSONToken])
    go (JSONTokenCloseBrace:tx1) acc = (JSONObject (reverse acc), tx1)
    go (JSONTokenComma:tx1) acc = go tx1 acc
    go ((JSONTokenText key):JSONTokenColon:tx1) acc = let (value, tx2) = parseTokens tx1
                                                      in go tx2 ((key, value):acc)
    go t tx1 = error ("Unexpected token in object" ++ show t ++ show tx1)

parseArray :: [JSONToken] -> (JSONValue, [JSONToken])
parseArray tokens = go tokens []
  where
    go :: [JSONToken] -> [JSONValue] -> (JSONValue, [JSONToken])
    go [] _ = error "Unexpected end of input"
    go (JSONTokenComma:tx2) acc = go tx2 acc
    go (JSONTokenCloseBracket:tx2) acc = (JSONArray (reverse acc), tx2)
    go tx acc = let (value, tx2) = parseTokens tx
                 in go tx2 (value:acc)

data JSONToken
  = JSONTokenOpenBrace -- {
  | JSONTokenCloseBrace -- }
  | JSONTokenColon
  | JSONTokenComma
  | JSONTokenText String
  | JSONTokenNumber Double
  | JSONTokenBoolean Bool
  | JSONTokenNull
  | JSONTokenOpenBracket -- [
  | JSONTokenCloseBracket -- ]
  deriving (Show, Eq)

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
