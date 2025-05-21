module Compiler.Parser.Assertion
  ( assertParseSuccess
  , assertParseProblem
  , assertSQLExpressionParenthesized
  ) where


import Text.Megaparsec
import Test.Hspec
import qualified Formatter.SQL
import qualified Compiler.Parser.SQL
import qualified Compiler.Parser.Error as Error


type Parser = Parsec Error.Error String


assertParseSuccess :: (Show a, Eq a) => Parser a -> String -> a -> IO ()
assertParseSuccess parser sourceCode expectedResult =
  case parse parser "" sourceCode of
    Left err -> fail $ errorBundlePretty err
    Right val -> val `shouldBe` expectedResult


assertParseProblem :: (Show a, Eq a) => Parser a -> String -> String -> IO ()
assertParseProblem parser sourceCode expectedErrorMessage =
  case parse parser "" sourceCode of
    Left err -> errorBundlePretty err `shouldContain` expectedErrorMessage
    Right val -> fail "expected parser to fail"


assertSQLExpressionParenthesized :: String -> String -> IO ()
assertSQLExpressionParenthesized sourceCode parenthesized =
  case parse Compiler.Parser.SQL.expression "" sourceCode of
    Left err -> fail $ errorBundlePretty err
    Right val -> Formatter.SQL.formatExpressionWithExplicitParenthesis val `shouldBe` parenthesized
