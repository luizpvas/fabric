module Compiler.Parser.SQL (expression, Expression(..), Operator(..), BinaryOperator(..), TertiaryOperator(..)) where


-- Here are some interesting queries I found while writing this parser:
--
-- SELECT +++++++++++1
-- SELECT ~~~~~~~~~~~1
-- SELECT 'HELLO ' COLLATE RTRIM = 'HELLO'
-- SELECT NOT NULL NOTNULL IS NOT NULL


import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Control.Monad.Combinators.Expr as Expr
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


expression :: Parser Expression
expression =
  wrap <$> primaryExpression <* space <*> (unaryPostfix <|> binaryRight <|> pure id)
  where
    wrap :: Expression -> (Expression -> Expression) -> Expression
    wrap expr f = f expr


primaryExpression :: Parser Expression
primaryExpression =
  choice
    [ fmap Parenthesized (between (char '(') (char ')') expression)
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


operators :: [[Expr.Operator Parser Expression]]
operators =
  [ [ Expr.Prefix (Operator . BitwiseNot <$ char '~')
    , Expr.Prefix (Operator . Plus <$ char '+')
    , Expr.Prefix (Operator . Minus <$ char '-')
    ]
  ]


unaryPrefixBitwiseNot :: Parser Expression
unaryPrefixBitwiseNot =
  Operator . BitwiseNot <$ char '~' <*> expression


unaryPrefixPlus :: Parser Expression
unaryPrefixPlus =
  Operator . Plus <$ char '+' <*> expression


unaryPrefixMinus :: Parser Expression
unaryPrefixMinus =
  Operator . Minus <$ char '-' <*> expression


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
    , toNotMatch  <$ string' "match"  <* space <*> expression
    , toNotRegexp <$ string' "regexp" <* space <*> expression
    , toNotGlob   <$ string' "glob"   <* space <*> expression
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
    , toAnd                    <$ string' "and"    <* space <*> expression
    , toOr                     <$ string' "or"     <* space <*> expression
    , toMatch                  <$ string' "match"  <* space <*> expression
    , toRegexp                 <$ string' "regexp" <* space <*> expression
    , toGlob                   <$ string' "glob"   <* space <*> expression
    , toJsonExtractDoubleArrow <$ string "->>" <* space <*> expression
    , toJsonExtractSingleArrow <$ string "->"  <* space <*> expression
    , toStringConcatenation    <$ string "||"  <* space <*> expression
    , toBitwiseShiftLeft       <$ string "<<"  <* space <*> expression
    , toBitwiseShiftRight      <$ string ">>"  <* space <*> expression
    , toLessThanOrEqualTo      <$ string "<="  <* space <*> expression
    , toNotEquals              <$ string "<>"  <* space <*> expression
    , toNotEquals              <$ string "!="  <* space <*> expression
    , toGreaterThanOrEqualTo   <$ string ">="  <* space <*> expression
    , toEquals                 <$ string "=="  <* space <*> expression
    , toMultiplication         <$ string "*"   <* space <*> expression
    , toDivision               <$ string "/"   <* space <*> expression
    , toModulus                <$ string "%"   <* space <*> expression
    , toSum                    <$ string "+"   <* space <*> expression
    , toSubtraction            <$ string "-"   <* space <*> expression
    , toBitwiseAnd             <$ string "&"   <* space <*> expression
    , toBitwiseOr              <$ string "|"   <* space <*> expression
    , toLessThan               <$ string "<"   <* space <*> expression
    , toGreaterThan            <$ string ">"   <* space <*> expression
    , toEquals                 <$ string "="   <* space <*> expression
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
    , toIsDistinctFrom <$ string' "distinct" <* space1 <* string' "from" <* space <*> expression
    , toIs <$> expression
    ]
  where
    toIsDistinctFrom rhs lhs = Operator (IsDistinctFrom lhs rhs)
    toIs rhs lhs             = Operator (Is lhs rhs)


binaryRightIsNot :: Parser (Expression -> Expression)
binaryRightIsNot =
  choice
    [ toIsNotDistinctFrom <$ string' "distinct" <* space1 <* string' "from" <* space <*> expression
    , toIsNot <$> expression
    ]
  where
    toIsNot rhs lhs = Operator (IsNot lhs rhs)
    toIsNotDistinctFrom rhs lhs = Operator (IsNotDistinctFrom lhs rhs)
