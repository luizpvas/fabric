{-# LANGUAGE OverloadedStrings #-}
module Compiler.SQL.ParserSpec (spec) where


import Compiler.SQL.Parser
import Compiler.Parser.Assertion
import Test.Hspec


spec :: Spec
spec = do
  describe "expr" $ do
    it "parses a literal positive integer" $ do
      assertParseSuccess expr "9223372036854775807" (LiteralInt 9223372036854775807)

    it "parses a literal positive integer with explicit sign" $ do
      assertParseSuccess expr "+9223372036854775807" (LiteralInt 9223372036854775807)

    it "parses a literal negative integer" $ do
      assertParseSuccess expr "-9223372036854775808" (LiteralInt (-9223372036854775808))
