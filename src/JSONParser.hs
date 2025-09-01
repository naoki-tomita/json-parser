module JSONParser (parse, JSONValue(..), tokenize, splitFirstExceptEscape, splitNotNumber, JSONToken(..)) where
import Data.Char (isDigit)

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
parseTokens (JSONTokenBraceOpen:tx) = parseObject tx
parseTokens (JSONTokenText t:tx) = (JSONString t, tx)
parseTokens (JSONTokenNumber t:tx) = (JSONNumber t, tx)
parseTokens (JSONTokenBoolean t:tx) = (JSONBool t, tx)
parseTokens (t:_) = error("Unexpected token " ++ show t)
parseTokens [] = (JSONNull, [])

parseObject :: [JSONToken] -> (JSONValue, [JSONToken])
parseObject tx = go tx []
  where
    go :: [JSONToken] -> [(String, JSONValue)] -> (JSONValue, [JSONToken])
    go (JSONTokenBraceClose:tx1) acc = (JSONObject(reverse acc), tx1)
    go (JSONTokenComma:tx1) acc = go tx1 acc
    go ((JSONTokenText key):JSONTokenColon:tx1) acc = let (value, tx2) = parseTokens tx1
                                                      in go tx2 ((key, value):acc)
    go t tx1 = error ("Unexpected token in object" ++ show t ++ show tx1)

data JSONToken
  = JSONTokenBraceOpen
  | JSONTokenBraceClose
  | JSONTokenColon
  | JSONTokenComma
  | JSONTokenText String
  | JSONTokenNumber Double
  | JSONTokenBoolean Bool
  | JSONTokenNull
  deriving (Show, Eq)

tokenize :: String -> [JSONToken]
tokenize ('{':xs) = JSONTokenBraceOpen : tokenize xs
tokenize ('}':xs) = JSONTokenBraceClose : tokenize xs
tokenize (':':xs) = JSONTokenColon : tokenize xs
tokenize (' ':xs) = tokenize xs
tokenize (',':xs) = JSONTokenComma : tokenize xs
tokenize ('"':xs) = let (before, after) = splitFirstExceptEscape '"' xs
                         in JSONTokenText before:tokenize after
tokenize ('n':'u':'l':'l':xs) = JSONTokenNull : tokenize xs
tokenize ('t':'r':'u':'e':xs) = JSONTokenBoolean True : tokenize xs
tokenize ('f':'a':'l':'s':'e':xs) = JSONTokenBoolean False : tokenize xs
tokenize (x:xs)
  -- 数字トークンは数字かマイナスで始まる。ピリオド始まりはありえない。
  | isDigit x || x == '-' = let (before, after) = splitNotNumber xs
                 in JSONTokenNumber (read (x:before) :: Double) : tokenize after
  | otherwise = undefined
tokenize [] = []

splitNotNumber :: String -> (String, String)
splitNotNumber = go
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
