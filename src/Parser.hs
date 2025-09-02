module Parser (parse) where
import Types (JSONValue(..), JSONToken(..))
import Tokenizer (tokenize)

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
