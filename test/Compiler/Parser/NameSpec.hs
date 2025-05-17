module Compiler.Parser.NameSpec (spec) where

import Test.Hspec
import Compiler.Parser.Assertion
import qualified Compiler.Parser.Name as Name


spec :: Spec
spec = do
  describe "variable" $ do
    it "parses a sole variable" $ do
      assertParseSuccess Name.variable "x" (Name.Sole "x")

    it "parses a sole variable with underscore prefix" $ do
      assertParseSuccess Name.variable "_x" (Name.Sole "_x")

    it "parses a sole variable with underscore suffix" $ do
      assertParseSuccess Name.variable "x_" (Name.Sole "x_")

    it "parses a sole variable with underscore prefix and suffix" $ do
      assertParseSuccess Name.variable "_x_" (Name.Sole "_x_")

    it "parses a sole variable with uppercase letters" $ do
      assertParseSuccess Name.variable "fullName" (Name.Sole "fullName")

    it "parses a sole variable with uppercase letters and underscore prefix" $ do
      assertParseSuccess Name.variable "_X" (Name.Sole "_X")

    it "parses a sole variable with numbers after the first character" $ do
      assertParseSuccess Name.variable "x1" (Name.Sole "x1")

    it "parses sole variables with UTF-8 characters in it" $ do
      assertParseSuccess Name.variable "estáAtivo" (Name.Sole "estáAtivo")
