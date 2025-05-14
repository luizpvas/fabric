module Compiler.Parser.Assertion (assertParseSuccess) where

import Data.Text (Text)
import Text.Megaparsec
import Data.Void (Void)
import Test.Hspec


assertParseSuccess :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> IO ()
assertParseSuccess parser text expectedResult =
  case parse parser "" text of
    Left err -> fail $ errorBundlePretty err
    Right val -> val `shouldBe` expectedResult
