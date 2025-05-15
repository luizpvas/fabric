module Compiler.SQL.Parser where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char


-- PARSER


type Parser = Parsec Void String


-- EXPR


data Expr
  = LiteralInt Int
  | LiteralFloat Float
  | LiteralString String
  | LiteralNull
  | LiteralTrue
  | LiteralFalse
  | LiteralCurrentTime
  | LiteralCurrentDate
  | LiteralCurrentTimestamp
  | Operator Operator
  deriving (Show, Eq)

data Operator
  = Unary UnaryOperator
  | Binary BinaryOperator
  | Tertiary TertiaryOperator
  deriving (Show, Eq)

data UnaryOperator
  = PrefixBitwiseNot Expr
  | PrefixPositive Expr
  | PrefixNegative Expr
  | Collate Expr String
  | Escape Expr 
  | IsNull Expr
  | NotNull Expr
  deriving (Show, Eq)

data BinaryOperator
  = StringConcatenation Expr Expr
  | JsonExtractSingleArrow Expr Expr
  | JsonExtractDoubleArrow Expr Expr
  | Multiplication Expr Expr
  | Division Expr Expr
  | Modulus Expr Expr
  | Sum Expr Expr
  | Subtraction Expr Expr
  | BitwiseAnd Expr Expr
  | BitwiseOr Expr Expr
  | BitwiseShiftLeft Expr Expr
  | BitwiseShiftRight Expr Expr
  | LessThan Expr Expr
  | GreaterThan Expr Expr
  | LessThanOrEqual Expr Expr
  | GreaterThanOrEqual Expr Expr
  | Equals Expr Expr
  | DoubleEquals Expr Expr
  | NotEquals String Expr Expr
  | Is Expr Expr
  | IsNot Expr Expr
  | IsDistinctFrom Expr Expr
  | IsNotDistinctFrom Expr Expr
  | In Expr Expr
  | NotIn Expr Expr
  | Match Expr Expr
  | NotMatch Expr Expr
  | Like Expr Expr
  | NotLike Expr Expr
  | Regexp Expr Expr
  | NotRegexp Expr Expr
  | Glob Expr Expr
  | NotGlob Expr Expr
  | And Expr Expr
  | Or Expr Expr
  deriving (Show, Eq)

data TertiaryOperator
  = Between Expr Expr Expr
  deriving (Show, Eq)

expr :: Parser Expr
expr =
  choice
    [ literalNumber
    ]


-- LITERAL NUMBER


data LiteralNumberSign
  = Positive
  | Negative
  deriving (Show, Eq)

literalNumber :: Parser Expr
literalNumber =
  label "number" $
    choice
      [ toLiteral Negative <$ char '-' <*> digits <*> decimalDigits
      , toLiteral Positive <$ char '+' <*> digits <*> decimalDigits
      , toLiteral Positive <$> digits <*> decimalDigits
      ]
  where
    digits = (:) <$> digitChar <*> some underscoreSeparatedDigit
    underscoreSeparatedDigit = choice [ id <$ char '_' <*> digitChar, digitChar ]
    decimalDigits = choice [Just <$ char '.' <*> digits, pure Nothing]
    toLiteral sign intPart maybeDecimalPart =
      case maybeDecimalPart of
        Nothing ->
          let toSign = if sign == Negative then negate else id
           in LiteralInt (toSign (read intPart))
        Just decimalPart ->
          let toSign = if sign == Negative then negate else id
           in LiteralFloat (toSign (read (intPart ++ "." ++ decimalPart)))
