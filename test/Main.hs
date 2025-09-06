import JSON (parse, stringify)
import System.Exit qualified as Exit
import Test.HUnit
import Tokenizer (splitFirstExceptEscape, splitUntilNotNumber, tokenize)
import Types (JSONToken (..), JSONValue (..))

splitFirstExceptEscapeTest1 :: Test
splitFirstExceptEscapeTest1 = TestCase (assertEqual "指定した最初の文字で分割する" ("abc", "xyz") (splitFirstExceptEscape 'd' "abcdxyz"))

splitFirstExceptEscapeTest2 :: Test
splitFirstExceptEscapeTest2 = TestCase (assertEqual "文字が見つからなければ最後まで抽出して、後半は空になる" ("abcdxyz", "") (splitFirstExceptEscape 'p' "abcdxyz"))

splitFirstExceptEscapeTest3 :: Test
splitFirstExceptEscapeTest3 = TestCase (assertEqual "エスケープされた文字は無視される" ("abcdxy", "z") (splitFirstExceptEscape 'd' "abc\\dxydz"))

splitUntilNotNumberTest1 :: Test
splitUntilNotNumberTest1 = TestCase (assertEqual "文字列から数字部分と数字以外の部分で分割する" ("213", "ab12") (splitUntilNotNumber "213ab12"))

splitUntilNotNumberTest2 :: Test
splitUntilNotNumberTest2 = TestCase (assertEqual "文字列から数字（小数点を含む）部分と数字以外の部分で分割する" ("21.3", "ab12") (splitUntilNotNumber "21.3ab12"))

json :: String
json = "{ \"foo \": \"bar\", \"hoge\": \"fu\\\"ga\", \"a\": -23.2376, \"array\": [1, \"string\", {\"key\": \"value\"}, false], \"xxxxxx\": true }"
tokenizeTest1 :: Test
tokenizeTest1 =
    TestCase
        ( assertEqual
            "トークン化できる"
            [ JSONTokenOpenBrace
            , JSONTokenText "foo "
            , JSONTokenColon
            , JSONTokenText "bar"
            , JSONTokenComma
            , JSONTokenText "hoge"
            , JSONTokenColon
            , JSONTokenText "fu\"ga"
            , JSONTokenComma
            , JSONTokenText "a"
            , JSONTokenColon
            , JSONTokenNumber (-23.2376)
            , JSONTokenComma
            , JSONTokenText "array"
            , JSONTokenColon
            , JSONTokenOpenBracket
            , JSONTokenNumber 1
            , JSONTokenComma
            , JSONTokenText "string"
            , JSONTokenComma
            , JSONTokenOpenBrace
            , JSONTokenText "key"
            , JSONTokenColon
            , JSONTokenText "value"
            , JSONTokenCloseBrace
            , JSONTokenComma
            , JSONTokenBoolean False
            , JSONTokenCloseBracket
            , JSONTokenComma
            , JSONTokenText "xxxxxx"
            , JSONTokenColon
            , JSONTokenBoolean True
            , JSONTokenCloseBrace
            ]
            (tokenize json)
        )

parseTest1 :: Test
parseTest1 =
    TestCase
        ( assertEqual
            "パースできる"
            ( JSONObject
                [ ("foo ", JSONString "bar")
                , ("hoge", JSONString "fu\"ga")
                , ("a", JSONNumber (-23.2376))
                ,
                    ( "array"
                    , JSONArray
                        [ JSONNumber 1
                        , JSONString "string"
                        , JSONObject [("key", JSONString "value")]
                        , JSONBool False
                        ]
                    )
                , ("xxxxxx", JSONBool True)
                ]
            )
            (parse json)
        )

stringifiedJson :: String
stringifiedJson = "{\"foo \":\"bar\",\"hoge\":\"fu\\\"ga\",\"a\":-23.2376,\"array\":[1.0,\"string\",{\"key\":\"value\"},false],\"xxxxxx\":true}"
jsonObject :: JSONValue
jsonObject =
    JSONObject
        [ ("foo ", JSONString "bar")
        , ("hoge", JSONString "fu\"ga")
        , ("a", JSONNumber (-23.2376))
        ,
            ( "array"
            , JSONArray
                [ JSONNumber 1
                , JSONString "string"
                , JSONObject [("key", JSONString "value")]
                , JSONBool False
                ]
            )
        , ("xxxxxx", JSONBool True)
        ]
stringifyTest1 :: Test
stringifyTest1 =
    TestCase
        ( assertEqual
            "stringifyできる"
            stringifiedJson
            (stringify jsonObject)
        )

main :: IO ()
main = do
    result <-
        runTestTT
            ( TestList
                [ splitFirstExceptEscapeTest1
                , splitFirstExceptEscapeTest2
                , splitFirstExceptEscapeTest3
                , splitUntilNotNumberTest1
                , splitUntilNotNumberTest2
                , tokenizeTest1
                , parseTest1
                , stringifyTest1
                ]
            )
    if errors result + failures result > 0
        then Exit.exitFailure
        else Exit.exitSuccess
