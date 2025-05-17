{-# LANGUAGE OverloadedStrings #-}
module Compiler.SQL.ExpressionSpec (spec) where


import Compiler.SQL.Expression as Expression
import Compiler.Parser.Assertion
import Test.Hspec


spec :: Spec
spec = do
  describe "literal" $ do
    it "parses a literal positive integer" $ do
      assertParseSuccess Expression.parser "10" (Expression.LiteralInt 10)

    it "parses a literal positive integer with explicit sign" $ do
      assertParseSuccess Expression.parser "0xA" (Expression.LiteralHex 10)

    it "parses a literal negative integer" $ do
      assertParseSuccess Expression.parser "0.1" (Expression.LiteralFloat 0.1)

    it "parses a literal string" $ do
      assertParseSuccess Expression.parser "'hello'" (Expression.LiteralString "hello")

    it "parses a literal blob prefixed with uppercase X" $ do
      assertParseSuccess Expression.parser "X'53514C697465'" (Expression.LiteralBlob "53514C697465")

    it "parses a literal blob prefixed with lowercase x" $ do
      assertParseSuccess Expression.parser "x'53514C697465'" (Expression.LiteralBlob "53514C697465")

    it "parses literal null" $ do
      assertParseSuccess Expression.parser "NULL" Expression.LiteralNull

    it "parses literal TRUE" $ do
      assertParseSuccess Expression.parser "TRUE" Expression.LiteralTrue

    it "parses literal FALSE" $ do
      assertParseSuccess Expression.parser "FALSE" Expression.LiteralFalse

    it "parses literal CURRENT_TIME" $ do
      assertParseSuccess Expression.parser "CURRENT_TIME" Expression.LiteralCurrentTime

    it "parses literal CURRENT_DATE" $ do
      assertParseSuccess Expression.parser "CURRENT_DATE" Expression.LiteralCurrentDate

    it "parses literal CURRENT_TIMESTAMP" $ do
      assertParseSuccess Expression.parser "CURRENT_TIMESTAMP" Expression.LiteralCurrentTimestamp

