module Formatter.SQL (formatExpressionWithExplicitParenthesis) where


import Compiler.Parser.SQL
import Compiler.Parser.SQL.AST


formatExpressionWithExplicitParenthesis :: Expression -> String
formatExpressionWithExplicitParenthesis = f
  where
    f :: Expression -> String
    f expr =
      case expr of
        (Parenthesized e)            -> "(" ++ f e ++ ")"
        (BitwiseNot e)               -> "(" ++ "~" ++ f e ++ ")"
        (Plus e)                     -> "(" ++ "+" ++ f e ++ ")"
        (Minus e)                    -> "(" ++ "-" ++ f e ++ ")"
        (Not e)                      -> "(NOT " ++ f e ++ ")"
        (Collate name e)             -> "(" ++ f e ++ " COLLATE " ++ name ++ ")"
        (IsNull e)                   -> "(" ++ f e ++ " ISNULL)"
        (NotNull e)                  -> "(" ++ f e ++ " NOT NULL)"
        (StringConcatenation l r)    -> "(" ++ f l ++ " || " ++ f r ++ ")"
        (JsonExtractSingleArrow l r) -> "(" ++ f l ++ "->" ++ f r ++ ")"
        (JsonExtractDoubleArrow l r) -> "(" ++ f l ++ "->>" ++ f r ++ ")"
        (Multiplication l r)         -> "(" ++ f l ++ " * " ++ f r ++ ")"
        (Division l r)               -> "(" ++ f l ++ " / " ++ f r ++ ")"
        (Modulus l r)                -> "(" ++ f l ++ " % " ++ f r ++ ")"
        (Sum l r)                    -> "(" ++ f l ++ " + " ++ f r ++ ")"
        (Subtraction l r)            -> "(" ++ f l ++ " - " ++ f r ++ ")"
        (BitwiseAnd l r)             -> "(" ++ f l ++ " & " ++ f r ++ ")"
        (BitwiseOr l r)              -> "(" ++ f l ++ " | " ++ f r ++ ")"
        (BitwiseShiftLeft l r)       -> "(" ++ f l ++ " << " ++ f r ++ ")"
        (BitwiseShiftRight l r)      -> "(" ++ f l ++ " >> " ++ f r ++ ")"
        (LessThan l r)               -> "(" ++ f l ++ " < " ++ f r ++ ")"
        (LessThanOrEqualTo l r)      -> "(" ++ f l ++ " <= " ++ f r ++ ")"
        (GreaterThan l r)            -> "(" ++ f l ++ " > " ++ f r ++ ")"
        (GreaterThanOrEqualTo l r)   -> "(" ++ f l ++ " >= " ++ f r ++ ")"
        (Equals l r)                 -> "(" ++ f l ++ " = " ++ f r ++ ")"
        (NotEquals l r)              -> "(" ++ f l ++ " <> " ++ f r ++ ")"
        (Is l r)                     -> "(" ++ f l ++ " IS " ++ f r ++ ")"
        (IsNot l r)                  -> "(" ++ f l ++ " IS NOT " ++ f r ++ ")"
        (IsDistinctFrom l r)         -> "(" ++ f l ++ " IS DISTINCT FROM " ++ f r ++ ")"
        (IsNotDistinctFrom l r)      -> "(" ++ f l ++ " IS NOT DISTINCT FROM " ++ f r ++ ")"
        (And l r)                    -> "(" ++ f l ++ " AND " ++ f r ++ ")"
        (Or l r)                     -> "(" ++ f l ++ " OR " ++ f r ++ ")"
        (Match l r)                  -> "(" ++ f l ++ " MATCH " ++ f r ++ ")"
        (NotMatch l r)               -> "(" ++ f l ++ " NOT MATCH " ++ f r ++ ")"
        (Regexp l r)                 -> "(" ++ f l ++ " REGEXP " ++ f r ++ ")"
        (NotRegexp l r)              -> "(" ++ f l ++ " NOT REGEXP " ++ f r ++ ")"
        (Glob l r)                   -> "(" ++ f l ++ " GLOB " ++ f r ++ ")"
        (NotGlob l r)                -> "(" ++ f l ++ " NOT GLOB " ++ f r ++ ")"
        (Like l r e)                 -> "(" ++ f l ++ " LIKE " ++ f r ++ formatEscape e ++ ")"
        (NotLike l r e)              -> "(" ++ f l ++ " NOT LIKE " ++ f r ++ formatEscape e ++ ")"
        (Between l m r)              -> "(" ++ f l ++ " BETWEEN " ++ f m ++ " AND " ++ f r ++ ")"
        (NotBetween l m r)           -> "(" ++ f l ++ " NOT BETWEEN " ++ f m ++ " AND " ++ f r ++ ")"
        (LiteralInt i)               -> show i
        (LiteralHex h)               -> show h
        (LiteralFloat f')            -> show f'
        (LiteralString s)            -> "'" ++ s ++ "'"
        (LiteralBlob b)              -> show b
        LiteralNull                  -> "NULL"
        LiteralTrue                  -> "TRUE"
        LiteralFalse                 -> "FALSE"
        LiteralCurrentTime           -> "CURRENT_TIME"
        LiteralCurrentDate           -> "CURRENT_DATE"
        LiteralCurrentTimestamp      -> "CURRENT_TIMESTAMP"


formatEscape :: EscapeClause -> String
formatEscape (NoEscape) = ""
formatEscape (Escape e) = " ESCAPE " ++ formatExpressionWithExplicitParenthesis e