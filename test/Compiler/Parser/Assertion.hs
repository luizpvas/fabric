module Compiler.Parser.Assertion
  ( assertParseSuccess
  , assertParseProblem
  , assertSQLExpressionParenthesized
  ) where


import Text.Megaparsec
import Data.Void (Void)
import Test.Hspec
import qualified Compiler.Parser.SQL
import qualified Formatter.SQL


assertParseSuccess :: (Show a, Eq a) => Parsec Void String a -> String -> a -> IO ()
assertParseSuccess parser sourceCode expectedResult =
  case parse parser "" sourceCode of
    Left err -> fail $ errorBundlePretty err
    Right val -> val `shouldBe` expectedResult


assertParseProblem :: (Show a, Eq a) => Parsec Void String a -> String -> String -> IO ()
assertParseProblem parser sourceCode expectedErrorMessage =
  case parse parser "" sourceCode of
    Left err -> errorBundlePretty err `shouldContain` expectedErrorMessage
    Right val -> fail "expected parser to fail"


assertSQLExpressionParenthesized :: String -> String -> IO ()
assertSQLExpressionParenthesized sourceCode parenthesized =
  case parse Compiler.Parser.SQL.expression "" sourceCode of
    Left err -> fail $ errorBundlePretty err
    Right val -> Formatter.SQL.formatExpressionWithExplicitParenthesis val `shouldBe` parenthesized
