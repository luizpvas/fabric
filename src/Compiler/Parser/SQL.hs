module Compiler.Parser.SQL (parser, Expression(..), Operator(..), BinaryOperator(..), TertiaryOperator(..)) where


-- Here are some interesting queries I found while writing this parser:
--
-- SELECT +++++++++++1
-- SELECT ~~~~~~~~~~~1
-- SELECT 'HELLO ' COLLATE RTRIM = 'HELLO'
-- SELECT NOT NULL NOTNULL IS NOT NULL


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
  = Parenthesized Expression
  | Operator Operator
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
  deriving (Show, Eq)


data BinaryOperator
  = In Expression Expression
  | NotIn Expression Expression
  | Like Expression Expression -- TODO: handle ESCAPE
  | NotLike Expression Expression
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
    [ fmap Parenthesized (between (char '(') (char ')') parser)
    , literalNumber
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
    [ string' "not" *> space *> unaryPostfixNot
    , toCollate <$ string' "collate" <* space1 <*> Name.variable
    , toIsNull  <$ string' "isnull"
    ]
  where
    toIsNull expr = Operator (IsNull expr)
    toCollate collationName expr = Operator (Collate collationName expr)


unaryPostfixNot :: Parser (Expression -> Expression)
unaryPostfixNot =
  choice
    [ toNotNull   <$ string' "null"
    , toNotMatch  <$ string' "match"  <* space <*> parser
    , toNotRegexp <$ string' "regexp" <* space <*> parser
    , toNotGlob   <$ string' "glob"   <* space <*> parser
    ]
  where
    toNotNull           = Operator . NotNull
    toNotMatch rhs lhs  = Operator (NotMatch lhs rhs)
    toNotRegexp rhs lhs = Operator (NotRegexp lhs rhs)
    toNotGlob rhs lhs   = Operator (NotGlob lhs rhs)


binaryRight :: Parser (Expression -> Expression)
binaryRight =
  choice
    [ string' "is"  *> space *> binaryRightIs
    , toAnd                    <$ string' "and"    <* space <*> parser
    , toOr                     <$ string' "or"     <* space <*> parser
    , toMatch                  <$ string' "match"  <* space <*> parser
    , toRegexp                 <$ string' "regexp" <* space <*> parser
    , toGlob                   <$ string' "glob"   <* space <*> parser
    , toJsonExtractDoubleArrow <$ string "->>" <* space <*> parser
    , toJsonExtractSingleArrow <$ string "->"  <* space <*> parser
    , toStringConcatenation    <$ string "||"  <* space <*> parser
    , toBitwiseShiftLeft       <$ string "<<"  <* space <*> parser
    , toBitwiseShiftRight      <$ string ">>"  <* space <*> parser
    , toLessThanOrEqualTo      <$ string "<="  <* space <*> parser
    , toNotEquals              <$ string "<>"  <* space <*> parser
    , toNotEquals              <$ string "!="  <* space <*> parser
    , toGreaterThanOrEqualTo   <$ string ">="  <* space <*> parser
    , toEquals                 <$ string "=="  <* space <*> parser
    , toMultiplication         <$ string "*"   <* space <*> parser
    , toDivision               <$ string "/"   <* space <*> parser
    , toModulus                <$ string "%"   <* space <*> parser
    , toSum                    <$ string "+"   <* space <*> parser
    , toSubtraction            <$ string "-"   <* space <*> parser
    , toBitwiseAnd             <$ string "&"   <* space <*> parser
    , toBitwiseOr              <$ string "|"   <* space <*> parser
    , toLessThan               <$ string "<"   <* space <*> parser
    , toGreaterThan            <$ string ">"   <* space <*> parser
    , toEquals                 <$ string "="   <* space <*> parser
    ]
  where
    toAnd rhs lhs                    = Operator (And lhs rhs)
    toOr rhs lhs                     = Operator (Or lhs rhs)
    toMatch rhs lhs                  = Operator (Match lhs rhs)
    toRegexp rhs lhs                 = Operator (Regexp lhs rhs)
    toGlob rhs lhs                   = Operator (Glob lhs rhs)
    toJsonExtractDoubleArrow rhs lhs = Operator (JsonExtractDoubleArrow lhs rhs)
    toJsonExtractSingleArrow rhs lhs = Operator (JsonExtractSingleArrow lhs rhs)
    toStringConcatenation rhs lhs    = Operator (StringConcatenation lhs rhs)
    toBitwiseShiftLeft rhs lhs       = Operator (BitwiseShiftLeft lhs rhs)
    toBitwiseShiftRight rhs lhs      = Operator (BitwiseShiftRight lhs rhs)
    toLessThanOrEqualTo rhs lhs      = Operator (LessThanOrEqualTo lhs rhs)
    toNotEquals rhs lhs              = Operator (NotEquals lhs rhs)
    toGreaterThanOrEqualTo rhs lhs   = Operator (GreaterThanOrEqualTo lhs rhs)
    toEquals rhs lhs                 = Operator (Equals lhs rhs)
    toMultiplication rhs lhs         = Operator (Multiplication lhs rhs)
    toDivision rhs lhs               = Operator (Division lhs rhs)
    toModulus rhs lhs                = Operator (Modulus lhs rhs)
    toSum rhs lhs                    = Operator (Sum lhs rhs)
    toSubtraction rhs lhs            = Operator (Subtraction lhs rhs)
    toBitwiseAnd rhs lhs             = Operator (BitwiseAnd lhs rhs)
    toBitwiseOr rhs lhs              = Operator (BitwiseOr lhs rhs)
    toLessThan rhs lhs               = Operator (LessThan lhs rhs)
    toGreaterThan rhs lhs            = Operator (GreaterThan lhs rhs)


binaryRightIs :: Parser (Expression -> Expression)
binaryRightIs =
  choice
    [ string' "not" *> space *> binaryRightIsNot
    , toIsDistinctFrom <$ string' "distinct" <* space1 <* string' "from" <* space <*> parser
    , toIs <$> parser
    ]
  where
    toIsDistinctFrom rhs lhs = Operator (IsDistinctFrom lhs rhs)
    toIs rhs lhs             = Operator (Is lhs rhs)


binaryRightIsNot :: Parser (Expression -> Expression)
binaryRightIsNot =
  choice
    [ toIsNotDistinctFrom <$ string' "distinct" <* space1 <* string' "from" <* space <*> parser
    , toIsNot <$> parser
    ]
  where
    toIsNot rhs lhs = Operator (IsNot lhs rhs)
    toIsNotDistinctFrom rhs lhs = Operator (IsNotDistinctFrom lhs rhs)
