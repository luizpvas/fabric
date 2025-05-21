module Compiler.Parser.SQL.AST (Expression(..)) where


data Expression
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
  | Like Expression Expression -- TODO: handle ESCAPE
  | NotLike Expression Expression
  | In Expression Expression
  | NotIn Expression Expression
  deriving (Show, Eq)

-- data BinaryOperator
--   deriving (Show, Eq)

-- data TertiaryOperator
--   = Between Expression Expression Expression
--   deriving (Show, Eq)


