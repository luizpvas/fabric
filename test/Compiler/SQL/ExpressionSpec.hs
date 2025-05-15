{-# LANGUAGE OverloadedStrings #-}
module Compiler.SQL.ExpressionSpec (spec) where


import Compiler.SQL.Expression as Expression
import Compiler.Parser.Assertion
import Test.Hspec


spec :: Spec
spec = do
  describe "expr" $ do
    it "parses a literal positive integer" $ do
      assertParseSuccess Expression.parser "10" (Expression.LiteralInt 10)

    it "parses a literal positive integer with explicit sign" $ do
      assertParseSuccess Expression.parser "0xA" (Expression.LiteralHex 10)

    it "parses a literal negative integer" $ do
      assertParseSuccess Expression.parser "0.1" (Expression.LiteralFloat 0.1)
