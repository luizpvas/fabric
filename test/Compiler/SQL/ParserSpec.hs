{-# LANGUAGE OverloadedStrings #-}
module Compiler.SQL.ParserSpec (spec) where


import Compiler.SQL.Parser
import Data.Text
import Data.Void
import Test.Hspec
import Text.Megaparsec


spec :: Spec
spec = do
  describe "expr" $ do
    it "parses a literal positive integer" $ do
      assertSuccess expr "9223372036854775807" (LiteralInt 9223372036854775807)

    it "parses a literal positive integer with explicit sign" $ do
      assertSuccess expr "+9223372036854775807" (LiteralInt 9223372036854775807)

    it "parses a literal negative integer" $ do
      assertSuccess expr "-9223372036854775808" (LiteralInt (-9223372036854775808))


assertSuccess :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> IO ()
assertSuccess parser text expectedResult =
  case parse parser "" text of
    Left err -> fail $ errorBundlePretty err
    Right val -> val `shouldBe` expectedResult
