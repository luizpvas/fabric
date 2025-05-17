module Compiler.Parser.NumberSpec (spec) where


import Compiler.Parser.Assertion
import Test.Hspec
import qualified Compiler.Parser.Number as Number


spec :: Spec
spec = do
  describe "int" $ do
    it "parses literal ints" $ do
      assertParseSuccess Number.parser "9223372036854775807" $
        (Number.Int 9223372036854775807)

    it "parses literal ints with one leading zero" $ do
        assertParseSuccess Number.parser "01" (Number.Int 1)

    it "parses literal ints with multiple leading zeros" $ do
        assertParseSuccess Number.parser "0001" (Number.Int 1)

    it "parses literal ints with underscore formatting" $
      assertParseSuccess Number.parser "1_000_000" $
        (Number.Int 1000000)

    it "fails to parse a literal positive int with trailing underscore" $ do
      assertParseProblem Number.parser "1_" "expecting digit"

  describe "hex" $ do
    it "parses literal hex numbers prefixed with 0x (lowercase x)" $ do
      assertParseSuccess Number.parser "0xA" (Number.Hex 10)

    it "parses literal hex numbers prefixed with 0X (uppercase X)" $ do
      assertParseSuccess Number.parser "0X10" (Number.Hex 16)

    it "parses literal hex numbers with underscore formatting" $ do
      assertParseSuccess Number.parser "0xA_000" (Number.Hex 40960)

  describe "float" $ do
    it "parses floats with int digits" $ do
      assertParseSuccess Number.parser "0.00001" (Number.Float 0.00001)

    it "parses floats with leading period" $ do
      assertParseSuccess Number.parser ".3" (Number.Float 0.3)

    it "parses floats with leading period and underscore formatting" $ do
      assertParseSuccess Number.parser ".000_001" (Number.Float 0.000001)

    it "parses floats with int digits only and exponential" $ do
      assertParseSuccess Number.parser "1e3" (Number.Float 1000)

    it "parses floats with exponential capital E" $ do
      assertParseSuccess Number.parser "1E3" (Number.Float 1000)

    it "parses floats with no int digits and exponential" $ do
      assertParseSuccess Number.parser ".3e2" (Number.Float 30)

    it "parses floats with no int digits and exponential formatted with underscore" $ do
      assertParseSuccess Number.parser ".3e1_0" (Number.Float 3e9)

    it "parses floats with exponential prefixed by plus sign" $ do
      assertParseSuccess Number.parser ".3e+1" (Number.Float 3)

    it "parses floats with negative exponentials" $ do
      assertParseSuccess Number.parser ".5e-1" (Number.Float (0.05))
