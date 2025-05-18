module Compiler.Parser.SQL.PrecedenceSpec where


import Test.Hspec
import Compiler.Parser.Assertion


spec :: Spec
spec =
  describe "~" $ do
    it "~"                    $ do assertParseSQLExpression "~~2" "(~(~2))"
    it "+"                    $ do assertParseSQLExpression "~+2" "(~(+2))"
    it "-"                    $ do assertParseSQLExpression "~-2" "(~(-2))"
    it "COLLATE"              $ do assertParseSQLExpression "~2 COLLATE NOCASE" "((~2) COLLATE NOCASE)"
    it "||"                   $ do assertParseSQLExpression "~2 || 'hello'" "((~2) || 'hello')"
    it "->"                   $ do assertParseSQLExpression "~2->'hello'" "((~2)->'hello')"
    it "->>"                  $ do assertParseSQLExpression "~2->>'hello'" "((~2)->>'hello')"
    it "*"                    $ do assertParseSQLExpression "~2 * 3" "((~2) * 3)"
    it "/"                    $ do assertParseSQLExpression "~2 / 3" "((~2) / 3)"
    it "%"                    $ do assertParseSQLExpression "~2 % 3" "((~2) % 3)"
    it "+"                    $ do assertParseSQLExpression "~2 + 3" "((~2) + 3)"
    it "-"                    $ do assertParseSQLExpression "~2 - 3" "((~2) - 3)"
    it "&"                    $ do assertParseSQLExpression "~2 & 3" "((~2) & 3)"
    it "|"                    $ do assertParseSQLExpression "~2 | 3" "((~2) | 3)"
    it "<<"                   $ do assertParseSQLExpression "~2 << 3" "((~2) << 3)"
    it ">>"                   $ do assertParseSQLExpression "~2 >> 3" "((~2) >> 3)"
    it "<"                    $ do assertParseSQLExpression "~2 < 3" "((~2) < 3)"
    it ">"                    $ do assertParseSQLExpression "~2 > 3" "((~2) > 3)"
    it "<="                   $ do assertParseSQLExpression "~2 <= 3" "((~2) <= 3)"
    it ">="                   $ do assertParseSQLExpression "~2 >= 3" "((~2) >= 3)"
    it "="                    $ do assertParseSQLExpression "~2 = 3" "((~2) = 3)"
    it "<>"                   $ do assertParseSQLExpression "~2 <> 3" "((~2) <> 3)"
    it "IS"                   $ do assertParseSQLExpression "~2 IS 3" "((~2) IS 3)"
    it "IS NOT"               $ do assertParseSQLExpression "~2 IS NOT 3" "((~2) IS NOT 3)"
    it "IS DISTINCT FROM"     $ do assertParseSQLExpression "~2 IS DISTINCT FROM 3" "((~2) IS DISTINCT FROM 3)"
    it "IS NOT DISTINCT FROM" $ do assertParseSQLExpression "~2 IS NOT DISTINCT FROM 3" "((~2) IS NOT DISTINCT FROM 3)"
    it "AND"                  $ do assertParseSQLExpression "~2 AND 3" "((~2) AND 3)"
    it "OR"                   $ do assertParseSQLExpression "~2 OR 3" "((~2) OR 3)"
