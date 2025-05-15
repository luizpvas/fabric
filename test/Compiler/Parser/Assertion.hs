module Compiler.Parser.Assertion (assertParseSuccess, assertParseProblem) where


import Text.Megaparsec
import Data.Void (Void)
import Test.Hspec


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