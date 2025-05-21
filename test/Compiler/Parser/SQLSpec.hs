module Compiler.Parser.SQLSpec (spec) where


import Compiler.Parser.SQL
import Compiler.Parser.SQL.AST
import Compiler.Parser.Assertion
import Test.Hspec


spec :: Spec
spec = do
  describe "literal" $ do
    it "parses literal positive integers" $ do
      assertParseSuccess expression "10" $
        LiteralInt 10

    it "parses literal hex numbers" $ do
      assertParseSuccess expression "0xA" $
        LiteralHex 10

    it "parses literal floats" $ do
      assertParseSuccess expression "0.1" $
        LiteralFloat 0.1

    it "parses literal strings" $ do
      assertParseSuccess expression "'hello'" $
        LiteralString "hello"

    it "parses literal blobs prefixed with uppercase X" $ do
      assertParseSuccess expression "X'53514C697465'" $
        LiteralBlob "53514C697465"

    it "parses literal blobs prefixed with lowercase x" $ do
      assertParseSuccess expression "x'53514C697465'" $
        LiteralBlob "53514C697465"

    it "parses literal NULL" $ do
      assertParseSuccess expression "NULL" LiteralNull

    it "parses literal TRUE" $ do
      assertParseSuccess expression "TRUE" LiteralTrue

    it "parses literal FALSE" $ do
      assertParseSuccess expression "FALSE" LiteralFalse

    it "parses literal CURRENT_TIME" $ do
      assertParseSuccess expression "CURRENT_TIME" $
        LiteralCurrentTime

    it "parses literal CURRENT_DATE" $ do
      assertParseSuccess expression "CURRENT_DATE" $
        LiteralCurrentDate

    it "parses literal CURRENT_TIMESTAMP" $ do
      assertParseSuccess expression "CURRENT_TIMESTAMP" $
        LiteralCurrentTimestamp

  describe "column names" $ do
    it "parses column names" $ do
      assertParseSuccess expression "email" $ (ColumnName "email")

    it "parses column names qualified with table name" $ do
      assertParseSuccess expression "users.email" $ (TableColumnName "users" "email")

    it "parses column names qualified with table name qualified with schema" $ do
      assertParseSuccess expression "public.users.email" $ (SchemaTableColumnName "public" "users" "email")

  describe "unary prefix" $ do
    it "parses bitwise-not operator" $ do
      assertParseSuccess expression "~1" $ BitwiseNot (LiteralInt 1)

    it "parses plus operator on non-numbers" $ do
      assertParseSuccess expression "+NULL" $ Plus (LiteralNull)

    it "parses plus operator on numbers" $ do
      assertParseSuccess expression "+1" $ Plus (LiteralInt 1)

    it "parses minus operator" $ do
      assertParseSuccess expression "-0.1" $ Minus (LiteralFloat 0.1)

    it "parses not operator" $ do
      assertParseSuccess expression "NOT TRUE" $ Not LiteralTrue

  describe "unary postfix" $ do
    it "parses COLLATE operator" $ do
      assertParseSuccess expression "'hello' COLLATE NOCASE" $
        Collate "NOCASE" (LiteralString "hello")

    it "parses ISNULL operator" $ do
      assertParseSuccess expression "NULL ISNULL" $ IsNull LiteralNull

    it "parses NOTNULL operator" $ do
      assertParseSuccess expression "NULL NOTNULL" $ NotNull LiteralNull

    it "parses NOT NULL operator" $ do
      assertParseSuccess expression "NULL NOT NULL" $ NotNull LiteralNull

  describe "binary" $ do
    it "parses string concatenation" $ do
      assertParseSuccess expression "'x' || 'y'" $
        StringConcatenation (LiteralString "x") (LiteralString "y")

    it "parses JSON extract single arrow" $ do
      assertParseSuccess expression "'{}'->'$'" $
        JsonExtractSingleArrow (LiteralString "{}") (LiteralString "$")

    it "parses JSON extract double arrow" $ do
      assertParseSuccess expression "'[0,1,2]'->>2" $
        JsonExtractDoubleArrow (LiteralString "[0,1,2]") (LiteralInt 2)

    it "parses multiplication" $ do
      assertParseSuccess expression "1 * 2" $
        Multiplication (LiteralInt 1) (LiteralInt 2)

    it "parses division" $ do
      assertParseSuccess expression "1 / 2" $
        Division (LiteralInt 1) (LiteralInt 2)

    it "parses modulus (reminder)" $ do
      assertParseSuccess expression "1 % 2" $
        Modulus (LiteralInt 1) (LiteralInt 2)

    it "parses sum" $ do
      assertParseSuccess expression "1 + 2" $
        Sum (LiteralInt 1) (LiteralInt 2)

    it "parses subtraction" $ do
      assertParseSuccess expression "1 - 2" $
        Subtraction (LiteralInt 1) (LiteralInt 2)

    it "parses bitwise and" $ do
      assertParseSuccess expression "1 & 2" $
        BitwiseAnd (LiteralInt 1) (LiteralInt 2)

    it "parses bitwise or" $ do
      assertParseSuccess expression "1 | 2" $
        BitwiseOr (LiteralInt 1) (LiteralInt 2)

    it "parses bitwise shift left" $ do
      assertParseSuccess expression "1 << 2" $
        BitwiseShiftLeft (LiteralInt 1) (LiteralInt 2)

    it "parses bitwise shift right" $ do
      assertParseSuccess expression "1 >> 2" $
        BitwiseShiftRight (LiteralInt 1) (LiteralInt 2)

    it "parses less than" $ do
      assertParseSuccess expression "1 < 2" $
        LessThan (LiteralInt 1) (LiteralInt 2)

    it "parses less than or equal to" $ do
      assertParseSuccess expression "1 <= 2" $
        LessThanOrEqualTo (LiteralInt 1) (LiteralInt 2)

    it "parses greater than" $ do
      assertParseSuccess expression "1 > 2" $
        GreaterThan (LiteralInt 1) (LiteralInt 2)

    it "parses greater than or equal to" $ do
      assertParseSuccess expression "1 >= 2" $
        GreaterThanOrEqualTo (LiteralInt 1) (LiteralInt 2)

    it "parses equals" $ do
      assertParseSuccess expression "1 = 2" $ do
        Equals (LiteralInt 1) (LiteralInt 2)

    it "parses double equals" $ do
      assertParseSuccess expression "1 == 2" $ do
        Equals (LiteralInt 1) (LiteralInt 2)

    it "parses not equals with <>" $ do
      assertParseSuccess expression "1 <> 2" $ do
        NotEquals (LiteralInt 1) (LiteralInt 2)

    it "parses not equals with !=" $ do
      assertParseSuccess expression "1 != 2" $ do
        NotEquals (LiteralInt 1) (LiteralInt 2)

    it "parses IS" $ do
      assertParseSuccess expression "1 IS 2" $ do
        Is (LiteralInt 1) (LiteralInt 2)

    it "parses IS NOT" $ do
      assertParseSuccess expression "1 IS NOT 2" $ do
        IsNot (LiteralInt 1) (LiteralInt 2)

    it "parses IS DISTINCT FROM" $ do
      assertParseSuccess expression "1 IS DISTINCT FROM 2" $ do
        IsDistinctFrom (LiteralInt 1) (LiteralInt 2)

    it "parses IS NOT DISTINCT FROM" $ do
      assertParseSuccess expression "1 IS NOT DISTINCT FROM 2" $ do
        IsNotDistinctFrom (LiteralInt 1) (LiteralInt 2)

    it "parses AND" $ do
      assertParseSuccess expression "1 AND 2" $
        And (LiteralInt 1) (LiteralInt 2)

    it "parses OR " $ do
      assertParseSuccess expression "1 OR 2" $
        Or (LiteralInt 1) (LiteralInt 2)

    it "parses MATCH" $ do
      assertParseSuccess expression "1 MATCH 2" $
        Match (LiteralInt 1) (LiteralInt 2)

    it "parses NOT MATCH" $ do
      assertParseSuccess expression "1 NOT MATCH 2" $
        NotMatch (LiteralInt 1) (LiteralInt 2)

    it "parses REGEXP" $ do
      assertParseSuccess expression "'12' REGEXP '\\d'" $
        Regexp (LiteralString "12") (LiteralString "\\d")

    it "parses NOT REGEXP" $ do
      assertParseSuccess expression "'12' NOT REGEXP '\\d'" $
        NotRegexp (LiteralString "12") (LiteralString "\\d")

    it "parses GLOB" $ do
      assertParseSuccess expression "'x' GLOB 'y'" $
        Glob (LiteralString "x") (LiteralString "y")

    it "parses NOT GLOB" $ do
      assertParseSuccess expression "'x' NOT GLOB 'y'" $
        NotGlob (LiteralString "x") (LiteralString "y")

    it "parses LIKE" $ do
      assertParseSuccess expression "'x' LIKE 'y'" $
        Like (LiteralString "x") (LiteralString "y") NoEscape

    it "parses LIKE with ESCAPE" $ do
      assertParseSuccess expression "'x' LIKE 'y' ESCAPE 'z'" $
        Like (LiteralString "x") (LiteralString "y") (Escape (LiteralString "z"))

    it "parses NOT LIKE" $ do
      assertParseSuccess expression "'x' NOT LIKE 'y'" $
        NotLike (LiteralString "x") (LiteralString "y") NoEscape

    it "parses NOT LIKE with ESCAPE" $ do
      assertParseSuccess expression "'x' NOT LIKE 'y' ESCAPE 'z'" $
        NotLike (LiteralString "x") (LiteralString "y") (Escape (LiteralString "z"))

    it "parses BETWEEN" $ do
      assertParseSuccess expression "2 BETWEEN 1 AND 3" $
        Between (LiteralInt 2) (LiteralInt 1) (LiteralInt 3)

    it "parses NOT BETWEEN" $ do
      assertParseSuccess expression "2 NOT BETWEEN 1 AND 3" $
        NotBetween (LiteralInt 2) (LiteralInt 1) (LiteralInt 3)

  describe "parenthesized" $ do
    it "parses parenthesized expressions" $ do
      assertParseSuccess expression "(1)" $
        Parenthesized (LiteralInt 1)

    it "parses deeply parenthesized expressions" $ do
      assertParseSuccess expression "(((NULL)))" $
        Parenthesized (Parenthesized (Parenthesized LiteralNull))

    it "parses parenthesized subexpressions" $ do
      assertParseSuccess expression "((1*2)+(3*4))" $
        Parenthesized (
          Sum
            (Parenthesized (Multiplication (LiteralInt 1) (LiteralInt 2)))
            (Parenthesized (Multiplication (LiteralInt 3) (LiteralInt 4)))
        )

  describe "precedence" $ do
    it "parses math expressions" $ do
      assertSQLExpressionParenthesized "1 + 2 * 3 > 5" "((1 + (2 * 3)) > 5)"

    it "parses conditionals with AND and OR" $ do
      assertSQLExpressionParenthesized "1=1 AND 2=2 OR 3=3" "(((1 = 1) AND (2 = 2)) OR (3 = 3))"

    it "parses multiple unary ~ prefixes" $ do
      assertSQLExpressionParenthesized "~~~3" "(~(~(~3)))"

    it "parses multiple unary NOT prefixes" $ do
      assertSQLExpressionParenthesized "NOT NOT NOT 3" "(NOT (NOT (NOT 3)))"

    it "parses like with escape" $ do
      assertSQLExpressionParenthesized "1 LIKE 2 ESCAPE 1 LIKE 2" $
        "((1 LIKE 2 ESCAPE 1) LIKE 2)"
