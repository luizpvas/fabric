module Compiler.Parser.NameSpec (spec) where

import Test.Hspec
import Compiler.Parser.Assertion
import qualified Compiler.Parser.Name as Name


spec :: Spec
spec = do
  describe "variable" $ do
    it "parses a sole variable" $ do
      assertParseSuccess Name.variable "x" "x"

    it "parses a sole variable with underscore prefix" $ do
      assertParseSuccess Name.variable "_x" "_x"

    it "parses a sole variable with underscore suffix" $ do
      assertParseSuccess Name.variable "x_" "x_"

    it "parses a sole variable with underscore prefix and suffix" $ do
      assertParseSuccess Name.variable "_x_" "_x_"

    it "parses a sole variable with uppercase letters" $ do
      assertParseSuccess Name.variable "fullName" "fullName"

    it "parses a sole variable with uppercase letters and underscore prefix" $ do
      assertParseSuccess Name.variable "_X" "_X"

    it "parses a sole variable with numbers after the first character" $ do
      assertParseSuccess Name.variable "x1" "x1"

    it "parses sole variables with UTF-8 characters in it" $ do
      assertParseSuccess Name.variable "estáAtivo" "estáAtivo"
