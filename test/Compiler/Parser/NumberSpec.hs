module Compiler.Parser.NumberSpec (spec) where


import Compiler.Parser.Assertion
import Test.Hspec
import qualified Compiler.Parser.Number as Number


spec :: Spec
spec = do
  describe "int" $ do
    it "parses literal positive integers" $ do
      assertParseSuccess Number.parser "9223372036854775807" $
        (Number.Int 9223372036854775807)

    it "parses literal positive integers with one leading zero" $ do
        assertParseSuccess Number.parser "01" (Number.Int 1)

    it "parses literal positive integers with multiple leading zeros" $ do
        assertParseSuccess Number.parser "0001" (Number.Int 1)

    it "parses literal positive integers with underscore formatting" $
      assertParseSuccess Number.parser "1_000_000" $
        (Number.Int 1000000)

    it "parses literal positive integers with explicit plus sign" $ do
      assertParseSuccess Number.parser "+9223372036854775807" $
        (Number.Int 9223372036854775807)

    it "parses literal negative integers with underscore formatting" $
      assertParseSuccess Number.parser "-9_000" $
        (Number.Int (-9000))

    it "parses literal negative integers" $ do
      assertParseSuccess Number.parser "-9223372036854775808" $
        (Number.Int (-9223372036854775808))

    it "fails to parse a literal positive integer with trailing underscore" $ do
      assertParseProblem Number.parser "1_" "expecting digit"

  describe "hex" $ do
    it "parses literal positive hex numbers prefixed with 0x (lowercase x)" $ do
      assertParseSuccess Number.parser "0xA"(Number.Hex 10)

    it "parses literal positive hex numbers prefixed with 0X (uppercase X)" $ do
      assertParseSuccess Number.parser "0X10"(Number.Hex 16)

    it "parses literal positive hex numbers prefixed by plus sign" $ do
      assertParseSuccess Number.parser "+0xA" (Number.Hex 10)

    it "parses literal positive hex numbers with underscore formatting" $ do
      assertParseSuccess Number.parser "0xA_000" (Number.Hex 40960)

    it "parses literal negative hex numbers" $ do
      assertParseSuccess Number.parser "-0xA" (Number.Hex (-10))

    it "parses literal negative hex numbers with underscore formatting" $ do
      assertParseSuccess Number.parser "-0x1_0" (Number.Hex (-16))
