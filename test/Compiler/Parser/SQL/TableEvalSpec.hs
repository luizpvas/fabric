module Compiler.Parser.SQL.TableEvalSpec (spec) where

import Compiler.Parser.SQL
import Compiler.Parser.SQL.AST
import Compiler.Parser.Assertion
import Test.Hspec


spec :: Spec
spec =
  describe "tableEval" $ do
    it "parses table names" $ do
      assertParseSuccess tableEval "email" (TableName "email")

    it "parses table names qualified with schema name" $ do
      assertParseSuccess tableEval "public.email" (SchemaTableName "public" "email")

    it "parses table functions without arguments" $ do
      assertParseSuccess tableEval "myUsers()" (TableFunction "myUsers" [])

    it "parses table functions with arguments" $ do
      assertParseSuccess tableEval "myUsers(1, true)" (TableFunction "myUsers" [ LiteralInt 1, LiteralTrue ])

    it "parses table functions qualified with schema name" $ do
      assertParseSuccess tableEval "public.myUsers(1, true)" (SchemaTableFunction "public" "myUsers" [ LiteralInt 1, LiteralTrue ])
