module Compiler.Parser.StringSpec (spec) where


import qualified Compiler.Parser.String as String
import Compiler.Parser.Assertion
import Test.Hspec


spec :: Spec
spec = do
  describe "singleQuoted" $ do
    it "parses strings with a single word" $ do
      assertParseSuccess String.singleQuoted "'Hello'" "Hello"

    it "parses strings with multiple words" $ do
      assertParseSuccess String.singleQuoted "'Hello, world!'" "Hello, world!"

    it "parses strings with UTF-8 characters" $ do
      assertParseSuccess String.singleQuoted "'não, 世界'" "não, 世界"

    it "parses strings with unicode emojis" $ do
      assertParseSuccess String.singleQuoted "'Hello 👍'" "Hello 👍"

    it "parses strings with escaped line break" $ do
      assertParseSuccess String.singleQuoted "'Hello \n world'" "Hello \n world"

    it "parses single quoted strings with escape character" $ do
      assertParseSuccess String.singleQuoted "'Hello '' world'" "Hello ' world"

  describe "doubleQuoted" $ do
    it "parses double quoted strings" $ do
      assertParseSuccess String.doubleQuoted "\"Hello\"" "Hello"

    it "parses double quoted strings with UTF-8 characters" $ do
      assertParseSuccess String.doubleQuoted "\"não\"" "não"

    it "parses double quoted strings with escaped double quote" $ do
      assertParseSuccess String.doubleQuoted "\"yes \\\" no\"" "yes \" no"
