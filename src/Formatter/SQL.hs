module Formatter.SQL (formatExpressionWithExplicitParenthesis) where


import Compiler.Parser.SQL
import Compiler.Parser.SQL.AST


formatExpressionWithExplicitParenthesis :: Expression -> String
formatExpressionWithExplicitParenthesis = f
  where
    f :: Expression -> String
    f expr =
      case expr of
        (Parenthesized e)                       -> "(" ++ f e ++ ")"
        (Operator (BitwiseNot e))               -> "(" ++ "~" ++ f e ++ ")"
        (Operator (Plus e))                     -> "(" ++ "+" ++ f e ++ ")"
        (Operator (Minus e))                    -> "(" ++ "-" ++ f e ++ ")"
        (Operator (Collate name e))             -> "(" ++ f e ++ " COLLATE " ++ name ++ ")"
        (Operator (IsNull e))                   -> "(" ++ f e ++ " ISNULL)"
        (Operator (NotNull e))                  -> "(" ++ f e ++ " NOT NULL)"
        (Operator (StringConcatenation l r))    -> "(" ++ f l ++ " || " ++ f r ++ ")"
        (Operator (JsonExtractSingleArrow l r)) -> "(" ++ f l ++ "->" ++ f r ++ ")"
        (Operator (JsonExtractDoubleArrow l r)) -> "(" ++ f l ++ "->>" ++ f r ++ ")"
        (Operator (Multiplication l r))         -> "(" ++ f l ++ " * " ++ f r ++ ")"
        (Operator (Division l r))               -> "(" ++ f l ++ " / " ++ f r ++ ")"
        (Operator (Modulus l r))                -> "(" ++ f l ++ " % " ++ f r ++ ")"
        (Operator (Sum l r))                    -> "(" ++ f l ++ " + " ++ f r ++ ")"
        (Operator (Subtraction l r))            -> "(" ++ f l ++ " - " ++ f r ++ ")"
        (Operator (BitwiseAnd l r))             -> "(" ++ f l ++ " & " ++ f r ++ ")"
        (Operator (BitwiseOr l r))              -> "(" ++ f l ++ " | " ++ f r ++ ")"
        (Operator (BitwiseShiftLeft l r))       -> "(" ++ f l ++ " << " ++ f r ++ ")"
        (Operator (BitwiseShiftRight l r))      -> "(" ++ f l ++ " >> " ++ f r ++ ")"
        (Operator (LessThan l r))               -> "(" ++ f l ++ " < " ++ f r ++ ")"
        (Operator (LessThanOrEqualTo l r))      -> "(" ++ f l ++ " <= " ++ f r ++ ")"
        (Operator (GreaterThan l r))            -> "(" ++ f l ++ " > " ++ f r ++ ")"
        (Operator (GreaterThanOrEqualTo l r))   -> "(" ++ f l ++ " >= " ++ f r ++ ")"
        (Operator (Equals l r))                 -> "(" ++ f l ++ " = " ++ f r ++ ")"
        (Operator (NotEquals l r))              -> "(" ++ f l ++ " <> " ++ f r ++ ")"
        (Operator (Is l r))                     -> "(" ++ f l ++ " IS " ++ f r ++ ")"
        (Operator (IsNot l r))                  -> "(" ++ f l ++ " IS NOT " ++ f r ++ ")"
        (Operator (IsDistinctFrom l r))         -> "(" ++ f l ++ " IS DISTINCT FROM " ++ f r ++ ")"
        (Operator (IsNotDistinctFrom l r))      -> "(" ++ f l ++ " IS NOT DISTINCT FROM " ++ f r ++ ")"
        (Operator (And l r))                    -> "(" ++ f l ++ " AND " ++ f r ++ ")"
        (Operator (Or l r))                     -> "(" ++ f l ++ " OR " ++ f r ++ ")"
        (Operator (Match l r))                  -> "(" ++ f l ++ " MATCH " ++ f r ++ ")"
        (Operator (NotMatch l r))               -> "(" ++ f l ++ " NOT MATCH " ++ f r ++ ")"
        (Operator (Regexp l r))                 -> "(" ++ f l ++ " REGEXP " ++ f r ++ ")"
        (Operator (NotRegexp l r))              -> "(" ++ f l ++ " NOT REGEXP " ++ f r ++ ")"
        (Operator (Glob l r))                   -> "(" ++ f l ++ " GLOB " ++ f r ++ ")"
        (Operator (NotGlob l r))                -> "(" ++ f l ++ " NOT GLOB " ++ f r ++ ")"
        (LiteralInt i)                          -> show i
        (LiteralHex h)                          -> show h
        (LiteralFloat f')                       -> show f'
        (LiteralString s)                       -> "'" ++ s ++ "'"
        (LiteralBlob b)                         -> show b
        LiteralNull                             -> "NULL"
        LiteralTrue                             -> "TRUE"
        LiteralFalse                            -> "FALSE"
        LiteralCurrentTime                      -> "CURRENT_TIME"
        LiteralCurrentDate                      -> "CURRENT_DATE"
        LiteralCurrentTimestamp                 -> "CURRENT_TIMESTAMP"