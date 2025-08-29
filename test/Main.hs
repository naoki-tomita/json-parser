import Test.HUnit
import JSONParser (splitFirstExceptEscape, tokenize, splitNotNumber, JSONValue(..), JSONToken(..), parse)
import qualified System.Exit as Exit

splitFirstExceptEscapeTest1 :: Test
splitFirstExceptEscapeTest1 = TestCase (assertEqual "指定した最初の文字で分割する" ("abc", "xyz") (splitFirstExceptEscape 'd' "abcdxyz"))

splitFirstExceptEscapeTest2 :: Test
splitFirstExceptEscapeTest2 = TestCase (assertEqual "文字が見つからなければ最後まで抽出して、後半は空になる" ("abcdxyz", "") (splitFirstExceptEscape 'p' "abcdxyz"))


splitFirstExceptEscapeTest3 :: Test
splitFirstExceptEscapeTest3 = TestCase (assertEqual "エスケープされた文字は無視される" ("abcdxy", "z") (splitFirstExceptEscape 'd' "abc\\dxydz"))

splitNotNumberTest1 :: Test
splitNotNumberTest1 = TestCase (assertEqual "文字列から数字部分と数字以外の部分で分割する" ("213", "ab12") (splitNotNumber "213ab12"))

splitNotNumberTest2 :: Test
splitNotNumberTest2 = TestCase (assertEqual "文字列から数字（小数点を含む）部分と数字以外の部分で分割する" ("21.3", "ab12") (splitNotNumber "21.3ab12"))

json :: String
json = "{ \"foo \": \"bar\", \"hoge\": \"fu\\\"ga\", \"a\": -23.2376, \"xxxxxx\": true }"
tokenizeTest1 :: Test
tokenizeTest1 = TestCase (assertEqual
                            "トークン化できる"
                            [JSONTokenBraceOpen,
                              JSONTokenText "foo ", JSONTokenColon, JSONTokenText "bar", JSONTokenComma,
                              JSONTokenText "hoge", JSONTokenColon, JSONTokenText "fu\"ga", JSONTokenComma,
                              JSONTokenText "a", JSONTokenColon, JSONTokenNumber (-23.2376), JSONTokenComma,
                              JSONTokenText "xxxxxx", JSONTokenColon, JSONTokenBoolean True,
                              JSONTokenBraceClose]
                            (tokenize json))

parseTest1 :: Test
parseTest1 = TestCase (assertEqual "パースできる" (JSONObject [("foo ", JSONString "bar"), ("hoge", JSONString "fu\"ga"), ("a", JSONNumber (-23.2376)), ("xxxxxx", JSONBool True)]) (parse json))

main :: IO ()
main = do
  result <- runTestTT (TestList [
    splitFirstExceptEscapeTest1,
    splitFirstExceptEscapeTest2,
    splitFirstExceptEscapeTest3,
    splitNotNumberTest1,
    splitNotNumberTest2,
    tokenizeTest1,
    parseTest1
    ])
  if errors result + failures result > 0
    then Exit.exitFailure
    else Exit.exitSuccess
