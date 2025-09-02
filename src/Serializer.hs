module Serializer (stringify) where
import Types (JSONValue (..))
import Data.List (intercalate, isPrefixOf)

stringify :: JSONValue -> String
stringify (JSONString text) = "\"" ++ replace "\"" "\\\"" text ++ "\""
stringify (JSONObject kvList) = "{" ++ intercalate "," (map kvStringify kvList) ++ "}"
stringify (JSONBool bool) = if bool then "true" else "false"
stringify (JSONNumber num) = show num
stringify (JSONArray arr) = "[" ++ intercalate "," (map stringify arr) ++ "]"
stringify _ = ""

kvStringify :: (String, JSONValue) -> String
kvStringify (k, v) = "\"" ++ k ++ "\"" ++ ":" ++ stringify v

replace :: String -> String -> String -> String
replace old new = go
  where
    go :: String -> String
    go str
      | str == "" = ""
      | old == "" = str
      | old `isPrefixOf` str = new ++ go (drop (length old) str)
      | otherwise = head str : go (tail str)
