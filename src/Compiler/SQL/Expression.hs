module Compiler.SQL.Expression (parser, Expression(..), Operator(..), BinaryOperator(..), TertiaryOperator(..)) where

-- Here are some funny queries I found along the way. Can you guess their result?
-- SELECT +++++++++++1;
-- SELECT ~~~~~~~~~~~1;

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Compiler.Parser.Number as Number
import qualified Compiler.Parser.String as String


-- PARSER


type Parser = Parsec Void String


-- EXPR


data Expression
  = LiteralInt Int
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
  | Operator Operator
  deriving (Show, Eq)


data Operator
  = BitwiseNot Expression
  | Plus Expression
  | Minus Expression
  | Collate Expression String
  | Escape Expression 
  | IsNull Expression
  | NotNull Expression
  | Binary BinaryOperator
  | Tertiary TertiaryOperator
  deriving (Show, Eq)


data BinaryOperator
  = StringConcatenation Expression Expression
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
  | GreaterThan Expression Expression
  | LessThanOrEqual Expression Expression
  | GreaterThanOrEqual Expression Expression
  | Equals Expression Expression
  | DoubleEquals Expression Expression
  | NotEquals String Expression Expression
  | Is Expression Expression
  | IsNot Expression Expression
  | IsDistinctFrom Expression Expression
  | IsNotDistinctFrom Expression Expression
  | In Expression Expression
  | NotIn Expression Expression
  | Match Expression Expression
  | NotMatch Expression Expression
  | Like Expression Expression
  | NotLike Expression Expression
  | Regexp Expression Expression
  | NotRegexp Expression Expression
  | Glob Expression Expression
  | NotGlob Expression Expression
  | And Expression Expression
  | Or Expression Expression
  deriving (Show, Eq)


data TertiaryOperator
  = Between Expression Expression Expression
  deriving (Show, Eq)


parser :: Parser Expression
parser =
  choice
    [ literalNumber
    , literalString
    , literalBlob
    , literalNull
    , literalTrue
    , literalFalse
    , literalCurrent
    , unaryBitwiseNot
    , unaryPlus
    , unaryMinus
    ]


literalNumber :: Parser Expression
literalNumber =
  fmap toExpressionNumber Number.parser
  where
    toExpressionNumber (Number.Int i)   = LiteralInt i
    toExpressionNumber (Number.Hex h)   = LiteralHex h
    toExpressionNumber (Number.Float f) = LiteralFloat f


literalString :: Parser Expression
literalString =
  fmap LiteralString String.singleQuotedParser


literalBlob :: Parser Expression
literalBlob =
  LiteralBlob <$ char' 'x' <* char '\'' <*> many hexDigitChar <* char '\''


literalNull :: Parser Expression
literalNull =
  LiteralNull <$ string' "null"


literalTrue :: Parser Expression
literalTrue =
  LiteralTrue <$ string' "true"


literalFalse :: Parser Expression
literalFalse =
  LiteralFalse <$ string' "false"


literalCurrent :: Parser Expression
literalCurrent =
  string' "current_" *> trailing
  where
    trailing :: Parser Expression
    trailing = choice
      [ LiteralCurrentDate      <$ string' "date"
      , LiteralCurrentTimestamp <$ string' "timestamp"
      , LiteralCurrentTime      <$ string' "time"
      ]


unaryBitwiseNot :: Parser Expression
unaryBitwiseNot =
  (\e -> Operator (BitwiseNot e)) <$ char '~' <*> parser


unaryPlus :: Parser Expression
unaryPlus =
  (\e -> Operator (Plus e)) <$ char '+' <*> parser


unaryMinus :: Parser Expression
unaryMinus =
  (\e -> Operator (Minus e)) <$ char '-' <*> parser
