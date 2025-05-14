{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.NumberSpec (spec) where


import Compiler.Parser.Assertion
import Test.Hspec
import qualified Compiler.Parser.Number as Number


spec :: Spec
spec = do
  describe "parser" $ do
    it "parses a literal positive integer" $ do
      assertParseSuccess Number.parser "9223372036854775807" $
        (Number.Int 9223372036854775807)

    it "parses a literal positive integer with explicit sign" $ do
      assertParseSuccess Number.parser "+9223372036854775807" $
        (Number.Int 9223372036854775807)

    it "parses a literal negative integer" $ do
      assertParseSuccess Number.parser "-9223372036854775808" $
        (Number.Int (-9223372036854775808))

