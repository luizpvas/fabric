module Compiler.Parser.SQL.AST 
  ( Expression(..)
  , EscapeClause(..)
  , InRight(..)
  , TableEval(..)
  ) where


-- SELECT


data Select = Select
  { resultColumnList :: ResultColumnList
  }


data ResultColumnList
  = ResultColumnListDistinct [SelectResultColumn]
  | ResultColumnListAll [SelectResultColumn]


data SelectResultColumn
  = SelectResultColumnExpression Expression
  | SelectResultColumnAliasExpression Expression String
  | SelectResultColumnStar
  | SelectResultColumnTableStar String
  | SelectResultColumnSchemaTableStar String String
  deriving (Show, Eq)


-- EXPRESSION


data Expression
  -- CONTAINERS
  = Parenthesized Expression
  -- LITERAL
  | LiteralInt Int
  | LiteralHex Int
  | LiteralFloat Float
  | LiteralString String
  | LiteralBlob String
  | LiteralNull
  | LiteralTrue
  | LiteralFalse
  | LiteralCurrentTime
  | LiteralCurrentDate
  | LiteralCurrentTimestamp
  -- COLUMN NAME
  | ColumnName String
  | TableColumnName String String
  | SchemaTableColumnName String String String
  -- UNARY PREFIX
  | BitwiseNot Expression
  | Plus Expression
  | Minus Expression
  | Not Expression
  -- UNARY POSTFIX
  | Collate String Expression
  | IsNull Expression
  | NotNull Expression
  -- BINARY
  | StringConcatenation Expression Expression
  | JsonExtractSingleArrow Expression Expression
  | JsonExtractDoubleArrow Expression Expression
  | Multiplication Expression Expression
  | Division Expression Expression
  | Modulus Expression Expression
  | Sum Expression Expression
  | Subtraction Expression Expression
  | BitwiseAnd Expression Expression
  | BitwiseOr Expression Expression
  | BitwiseShiftLeft Expression Expression
  | BitwiseShiftRight Expression Expression
  | LessThan Expression Expression
  | LessThanOrEqualTo Expression Expression
  | GreaterThan Expression Expression
  | GreaterThanOrEqualTo Expression Expression
  | Equals Expression Expression
  | NotEquals Expression Expression
  | Is Expression Expression
  | IsNot Expression Expression
  | IsDistinctFrom Expression Expression
  | IsNotDistinctFrom Expression Expression
  | And Expression Expression
  | Or Expression Expression
  | Match Expression Expression
  | NotMatch Expression Expression
  | Regexp Expression Expression
  | NotRegexp Expression Expression
  | Glob Expression Expression
  | NotGlob Expression Expression
  | Like Expression Expression EscapeClause
  | NotLike Expression Expression EscapeClause
  | In Expression InRight
  | NotIn Expression InRight
  -- TERNARY
  | Between Expression Expression Expression
  | NotBetween Expression Expression Expression
  deriving (Show, Eq)


data InRight
  = InRightExpressionList [Expression]
  | InRightTableEval TableEval
  deriving (Show, Eq)


data TableEval
  = TableName String
  | SchemaTableName String String
  | TableFunction String [Expression]
  | SchemaTableFunction String String [Expression]
  deriving (Show, Eq)


data EscapeClause
  = NoEscape
  | Escape Expression
  deriving (Show, Eq)
