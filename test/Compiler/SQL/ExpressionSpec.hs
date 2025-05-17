{-# LANGUAGE OverloadedStrings #-}
module Compiler.SQL.ExpressionSpec (spec) where


import Compiler.SQL.Expression
import Compiler.Parser.Assertion
import Test.Hspec


spec :: Spec
spec = do
  describe "literal" $ do
    it "parses literal positive integers" $ do
      assertParseSuccess parser "10" $
        LiteralInt 10

    it "parses literal hex numbers" $ do
      assertParseSuccess parser "0xA" $
        LiteralHex 10

    it "parses literal floats" $ do
      assertParseSuccess parser "0.1" $
        LiteralFloat 0.1

    it "parses literal strings" $ do
      assertParseSuccess parser "'hello'" $
        LiteralString "hello"

    it "parses literal blobs prefixed with uppercase X" $ do
      assertParseSuccess parser "X'53514C697465'" $
        LiteralBlob "53514C697465"

    it "parses literal blobs prefixed with lowercase x" $ do
      assertParseSuccess parser "x'53514C697465'" $
        LiteralBlob "53514C697465"

    it "parses literal NULL" $ do
      assertParseSuccess parser "NULL" LiteralNull

    it "parses literal TRUE" $ do
      assertParseSuccess parser "TRUE" LiteralTrue

    it "parses literal FALSE" $ do
      assertParseSuccess parser "FALSE" LiteralFalse

    it "parses literal CURRENT_TIME" $ do
      assertParseSuccess parser "CURRENT_TIME" $
        LiteralCurrentTime

    it "parses literal CURRENT_DATE" $ do
      assertParseSuccess parser "CURRENT_DATE" $
        LiteralCurrentDate

    it "parses literal CURRENT_TIMESTAMP" $ do
      assertParseSuccess parser "CURRENT_TIMESTAMP" $
        LiteralCurrentTimestamp

  describe "unary prefix" $ do
    it "parses bitwise-not operator" $ do
      assertParseSuccess parser "~1" $
        Operator (BitwiseNot (LiteralInt 1))

    it "parses plus operator on non-numbers" $ do
      assertParseSuccess parser "+NULL" $
        Operator (Plus (LiteralNull))

    it "parses plus operator on numbers" $ do
      assertParseSuccess parser "+1" $
        Operator (Plus (LiteralInt 1))

    it "parses minus operator" $ do
      assertParseSuccess parser "-0.1" $
        Operator (Minus (LiteralFloat 0.1))

  describe "unary suffix" $ do
    it "parses COLLATE operator" $ do
      assertParseSuccess parser "'hello' COLLATE NOCASE" $
        Operator (Collate "NOCASE" (LiteralString "hello"))

    it "parses ISNULL operator" $ do
      assertParseSuccess parser "NULL ISNULL" $
        Operator (IsNull LiteralNull)

    it "parses NOTNULL operator" $ do
      assertParseSuccess parser "NULL NOTNULL" $
        Operator (NotNull LiteralNull)

    it "parses NOT NULL operator" $ do
      assertParseSuccess parser "NULL NOT NULL" $
        Operator (NotNull LiteralNull)

  describe "binary" $ do
    it "parses string concatenation" $ do
      assertParseSuccess parser "'x' || 'y'" $
        Operator (StringConcatenation (LiteralString "x") (LiteralString "y"))

    it "parses JSON extract single arrow" $ do
      assertParseSuccess parser "'{}'->'$'" $
        Operator (JsonExtractSingleArrow (LiteralString "{}") (LiteralString "$"))

    it "parses JSON extract double arrow" $ do
      assertParseSuccess parser "'[0,1,2]'->>2" $
        Operator (JsonExtractDoubleArrow (LiteralString "[0,1,2]") (LiteralInt 2))

    it "parses multiplication" $ do
      assertParseSuccess parser "1 * 2" $
        Operator (Multiplication (LiteralInt 1) (LiteralInt 2))

    it "parses division" $ do
      assertParseSuccess parser "1 / 2" $
        Operator (Division (LiteralInt 1) (LiteralInt 2))
