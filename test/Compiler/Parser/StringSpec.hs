module Compiler.Parser.StringSpec (spec) where


import qualified Compiler.Parser.String as String
import Compiler.Parser.Assertion
import Test.Hspec


spec :: Spec
spec =
  describe "singleQuotedParser" $ do
    it "parses strings with a single word" $ do
      assertParseSuccess String.singleQuotedParser "'Hello'" "Hello"

    it "parses strings with multiple words" $ do
      assertParseSuccess String.singleQuotedParser "'Hello, world!'" "Hello, world!"

    it "parses strings with UTF-8 characters" $ do
      assertParseSuccess String.singleQuotedParser "'não, 世界'" "não, 世界"

    it "parses strings with unicode emojis" $ do
      assertParseSuccess String.singleQuotedParser "'Hello 👍'" "Hello 👍"

    it "parses strings with escaped line break" $ do
      assertParseSuccess String.singleQuotedParser "'Hello \n world'" "Hello \n world"

    it "parses single quoted strings with escape character" $ do
      assertParseSuccess String.singleQuotedParser "'Hello '' world'" "Hello ' world"
