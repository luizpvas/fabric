{-# LANGUAGE OverloadedStrings #-}

module Compiler.SQL.ParserSpec where

import Compiler.SQL.Parser
import Test.Hspec
import Text.Megaparsec

spec :: Spec
spec = do
  describe "expr" $ do
    it "parses a literal positive integer" $ do
      case parse expr "" "123" of
        Left err -> fail $ errorBundlePretty err
        Right expr -> expr `shouldBe` LiteralInt 123

    it "parses a literal negative integer" $ do
      case parse expr "" "-9" of
        Left err -> fail $ errorBundlePretty err
        Right expr -> expr `shouldBe` LiteralInt (-9)
