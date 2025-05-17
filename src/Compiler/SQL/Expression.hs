module Compiler.SQL.Expression (parser, Expression(..), Operator(..), BinaryOperator(..), TertiaryOperator(..)) where


-- Here are some funny queries I found along the way. Can you guess their result?
-- SELECT +++++++++++1;
-- SELECT ~~~~~~~~~~~1;
-- SELECT 'HELLO ' COLLATE RTRIM = 'HELLO';
-- SELECT NOT NULL NOTNULL IS NOT NULL;


import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Compiler.Parser.Name as Name
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
  -- UNARY PREFIX
  = BitwiseNot Expression
  | Plus Expression
  | Minus Expression
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
  deriving (Show, Eq)


data BinaryOperator
  = Equals Expression Expression
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
  | Like Expression Expression -- TODO: handle ESCAPE
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
  (\expr f -> f expr) <$> primary <* space <*> (unaryPostfix <|> binaryRight <|> pure id)


primary :: Parser Expression
primary =
  choice
    [ literalNumber
    , literalString
    , literalBlob
    , literalNull
    , literalTrue
    , literalFalse
    , literalCurrent
    , unaryPrefixBitwiseNot
    , unaryPrefixPlus
    , unaryPrefixMinus
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


unaryPrefixBitwiseNot :: Parser Expression
unaryPrefixBitwiseNot =
  Operator . BitwiseNot <$ char '~' <*> parser


unaryPrefixPlus :: Parser Expression
unaryPrefixPlus =
  Operator . Plus <$ char '+' <*> parser


unaryPrefixMinus :: Parser Expression
unaryPrefixMinus =
  Operator . Minus <$ char '-' <*> parser


unaryPostfix :: Parser (Expression -> Expression)
unaryPostfix =
  choice
    [ toCollate <$ string' "collate" <* space1 <*> Name.variable
    , Operator . IsNull <$ string' "isnull"
    , unaryPostfixNot
    ]
  where
    toCollate collationName expr =
      Operator (Collate collationName expr)


unaryPostfixNot :: Parser (Expression -> Expression)
unaryPostfixNot =
  string' "not" *> trailing
  where
    trailing :: Parser (Expression -> Expression)
    trailing =
      choice
        [ Operator . NotNull <$ space <* string' "null"
        ]


binaryRight :: Parser (Expression -> Expression)
binaryRight =
  choice
    [ toStringConcatenation    <$ string "||"  <* space <*> parser
    , toJsonExtractDoubleArrow <$ string "->>" <* space <*> parser
    , toJsonExtractSingleArrow <$ string "->"  <* space <*> parser
    , toMultiplication         <$ string "*"   <* space <*> parser
    , toDivision               <$ string "/"   <* space <*> parser
    , toModulus                <$ string "%"   <* space <*> parser
    , toSum                    <$ string "+"   <* space <*> parser
    , toSubtraction            <$ string "-"   <* space <*> parser
    , toBitwiseAnd             <$ string "&"   <* space <*> parser
    , toBitwiseOr              <$ string "|"   <* space <*> parser
    , toBitwiseShiftLeft       <$ string "<<"  <* space <*> parser
    , toBitwiseShiftRight      <$ string ">>"  <* space <*> parser
    , toLessThanOrEqualTo      <$ string "<="  <* space <*> parser
    , toLessThan               <$ string "<"   <* space <*> parser
    , toGreaterThanOrEqualTo   <$ string ">="  <* space <*> parser
    , toGreaterThan            <$ string ">"   <* space <*> parser
    ]
  where
    toStringConcatenation rhs lhs    = Operator (StringConcatenation lhs rhs)
    toJsonExtractDoubleArrow rhs lhs = Operator (JsonExtractDoubleArrow lhs rhs)
    toJsonExtractSingleArrow rhs lhs = Operator (JsonExtractSingleArrow lhs rhs)
    toMultiplication rhs lhs         = Operator (Multiplication lhs rhs)
    toDivision rhs lhs               = Operator (Division lhs rhs)
    toModulus rhs lhs                = Operator (Modulus lhs rhs)
    toSum rhs lhs                    = Operator (Sum lhs rhs)
    toSubtraction rhs lhs            = Operator (Subtraction lhs rhs)
    toBitwiseAnd rhs lhs             = Operator (BitwiseAnd lhs rhs)
    toBitwiseOr rhs lhs              = Operator (BitwiseOr lhs rhs)
    toBitwiseShiftLeft rhs lhs       = Operator (BitwiseShiftLeft lhs rhs)
    toBitwiseShiftRight rhs lhs      = Operator (BitwiseShiftRight lhs rhs)
    toLessThan rhs lhs               = Operator (LessThan lhs rhs)
    toLessThanOrEqualTo rhs lhs      = Operator (LessThanOrEqualTo lhs rhs)
    toGreaterThan rhs lhs            = Operator (GreaterThan lhs rhs)
    toGreaterThanOrEqualTo rhs lhs   = Operator (GreaterThanOrEqualTo lhs rhs)
