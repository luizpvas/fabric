{-# LANGUAGE OverloadedStrings #-}
module Compiler.SQL.ExpressionSpec (spec) where


import Compiler.SQL.Expression as Expression
import Compiler.Parser.Assertion
import Test.Hspec


spec :: Spec
spec = do
  describe "literal" $ do
    it "parses literal positive integers" $ do
      assertParseSuccess Expression.parser "10" (Expression.LiteralInt 10)

    it "parses literal hex numbers" $ do
      assertParseSuccess Expression.parser "0xA" (Expression.LiteralHex 10)

    it "parses literal floats" $ do
      assertParseSuccess Expression.parser "0.1" (Expression.LiteralFloat 0.1)

    it "parses literal strings" $ do
      assertParseSuccess Expression.parser "'hello'" (Expression.LiteralString "hello")

    it "parses literal blobs prefixed with uppercase X" $ do
      assertParseSuccess Expression.parser "X'53514C697465'" (Expression.LiteralBlob "53514C697465")

    it "parses literal blobs prefixed with lowercase x" $ do
      assertParseSuccess Expression.parser "x'53514C697465'" (Expression.LiteralBlob "53514C697465")

    it "parses literal NULL" $ do
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

  describe "unary prefix" $ do
    it "parses bitwise-not operator" $ do
      assertParseSuccess Expression.parser "~1" (Expression.Operator (Expression.BitwiseNot (Expression.LiteralInt 1)))

    it "parses plus operator on non-numbers" $ do
      assertParseSuccess Expression.parser "+NULL" (Expression.Operator (Expression.Plus (Expression.LiteralNull)))

    it "parses plus operator on numbers" $ do
      assertParseSuccess Expression.parser "+1" (Expression.Operator (Expression.Plus (Expression.LiteralInt 1)))

    it "parses minus operator" $ do
      assertParseSuccess Expression.parser "-0.1" (Expression.Operator (Expression.Minus (Expression.LiteralFloat 0.1)))

  describe "unary suffix" $ do
    it "parses COLLATE operator" $ do
      assertParseSuccess Expression.parser "'hello' COLLATE NOCASE"
        (Expression.Operator (Expression.Collate "NOCASE" (Expression.LiteralString "hello")))
