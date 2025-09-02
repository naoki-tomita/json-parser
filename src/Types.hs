module Types (JSONValue(..), JSONToken(..)) where

data JSONValue
  = JSONString String
  | JSONNumber Double
  | JSONBool Bool
  | JSONNull
  | JSONArray [JSONValue]
  | JSONObject [(String, JSONValue)]
  deriving (Show, Eq)

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
