module Compiler.Parser.SQL
  ( expression
  ) where


-- Here are some interesting queries I found while writing this parser:
--
-- SELECT +++++++++++1
-- SELECT ~~~~~~~~~~~1
-- SELECT 'HELLO ' COLLATE RTRIM = 'HELLO'
-- SELECT NOT NULL NOTNULL IS NOT NULL


import Data.Void
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import Compiler.Parser.SQL.AST
import qualified Compiler.Parser.Name as Name
import qualified Compiler.Parser.Number as Number
import qualified Compiler.Parser.String as String


-- PARSER


type Parser = Parsec Void String


expression :: Parser Expression
expression = precedence9


-- PRIMARY: PRECEDENCE 0


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
    , fmap Parenthesized (between (char '(') (char ')') expression)
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


-- PRECEDENCE 1
-- ~[expr]
-- +[expr]
-- -[expr]


precedence1 :: Parser Expression
precedence1 =
  choice
    [ BitwiseNot <$ char '~' <*> precedence1 <* space
    , Plus       <$ char '+' <*> precedence1 <* space
    , Minus      <$ char '-' <*> precedence1 <* space
    , primary <* space
    ]


-- PRECEDENCE 2
-- [expr] COLLATE (collation-name)


precedence2 :: Parser Expression
precedence2 = do
  (\e f -> f e) <$> precedence1 <*> (collate <|> pure id)
  where
    collate :: Parser (Expression -> Expression)
    collate =
      Collate <$ string' "collate" <* space1 <*> Name.variable <* space


-- PRECEDENCE 3
-- [expr] || [expr]
-- [expr] -> [expr]
-- [expr] ->> [expr]


precedence3 :: Parser Expression
precedence3 = do
  left  <- precedence2
  pairs <- many ((,) <$> operator <*> precedence2)
  return $ foldl (\l (op, r) -> op l r) left pairs
  where
    operator :: Parser (Expression -> Expression -> Expression)
    operator = choice
      [ StringConcatenation    <$ string "||"  <* space
      , JsonExtractDoubleArrow <$ string "->>" <* space
      , JsonExtractSingleArrow <$ string "->"  <* space
      ]


-- PRECEDENCE 4
-- [expr] * [expr]
-- [expr] / [expr]
-- [expr] % [expr]


precedence4 :: Parser Expression
precedence4 = do
  left <- precedence3
  pairs <- many ((,) <$> operator <*> precedence3)
  return $ foldl (\l (op, r) -> op l r) left pairs
  where
    operator :: Parser (Expression -> Expression -> Expression)
    operator = choice
      [ Multiplication <$ string "*" <* space
      , Division       <$ string "/" <* space
      , Modulus        <$ string "%" <* space
      ]


-- PRECEDENCE 5
-- [expr] + [expr]
-- [expr] - [expr]


precedence5 :: Parser Expression
precedence5 = do
  left <- precedence4
  pairs <- many ((,) <$> operator <*> precedence4)
  return $ foldl (\l (op, r) -> op l r) left pairs
  where
    operator :: Parser (Expression -> Expression -> Expression)
    operator = choice
      [ Sum         <$ string "+" <* space
      , Subtraction <$ string "-" <* space
      ]


-- PRECEDENCE 6
-- [expr] & [expr]
-- [expr] | [expr]
-- [expr] << [expr]
-- [expr] >> [expr]


precedence6 :: Parser Expression
precedence6 = do
  left <- precedence5
  pairs <- many ((,) <$> operator <*> precedence5)
  return $ foldl (\l (op, r) -> op l r) left pairs
  where
    operator :: Parser (Expression -> Expression -> Expression)
    operator = choice
      [ BitwiseAnd        <$ string "&" <* space
      , BitwiseOr         <$ string "|" <* space
      , BitwiseShiftLeft  <$ string "<<" <* space
      , BitwiseShiftRight <$ string ">>" <* space
      ]


-- PRECEDENCE 7
-- [expr] ESCAPE [escape-character-expr]


precedence7 :: Parser Expression
precedence7 = precedence6


-- PRECEDENCE 8
-- [expr] < [expr]
-- [expr] > [expr]
-- [expr] <= [expr]
-- [expr] >= [expr]


precedence8 :: Parser Expression
precedence8 = do
  left <- precedence7
  pairs <- many ((,) <$> operator <*> precedence7)
  return $ foldl (\l (op, r) -> op l r) left pairs
  where
    operator :: Parser (Expression -> Expression -> Expression)
    operator = choice
      [ LessThanOrEqualTo    <$ string "<=" <* space
      , GreaterThanOrEqualTo <$ string ">=" <* space
      -- NOTE: try is necessary because the operator <> (not equals) have lower
      -- precedence than < (less than).
      , try (LessThan <$ string ">" <* notFollowedBy (char '<') <* space)
      , GreaterThan <$ string ">"  <* space
      ]


-- PRECEDENCE 9
-- [expr] = [expr]
-- [expr] == [expr]
-- [expr] <> [expr]
-- [expr] != [expr]
-- [expr] IS [expr]
-- [expr] IS NOT [expr]
-- [expr] IS DISTINCT FROM [expr]
-- [expr] IS NOT DISTINCT FROM [expr]
-- [expr] BETWEEN [expr] AND [expr]
-- [expr] IN [expr]
-- [expr] NOT IN [expr]
-- [expr] MATCH [expr]
-- [expr] NOT MATCH [expr]
-- [expr] LIKE [expr]
-- [expr] NOT LIKE [expr]
-- [expr] REGEXP [expr]
-- [expr] NOT REGEXP [expr]
-- [expr] GLOB [expr]
-- [expr] NOT GLOB [expr]
-- [expr] ISNULL
-- [expr] NOTNULL
-- [expr] NOT NULL


data Precedence9
  = Unary (Expression -> Expression)
  | Binary (Expression -> Expression -> Expression) Expression
  -- | Ternary Expression Expression (Expression -> Expression -> Expression -> Expression)


precedence9 :: Parser Expression
precedence9 = do
  left <- precedence8
  nexts <- many next
  return $ foldl eval left nexts
  where
    eval :: Expression -> Precedence9 -> Expression
    eval expr next =
      case next of
        Unary toExpr -> toExpr expr
        Binary toExpr right -> toExpr expr right

    next :: Parser Precedence9
    next =
      choice
        [ Unary <$> unary
        , Binary <$> binary <*> precedence8
        ]

    unary :: Parser (Expression -> Expression)
    unary =
      choice
        [ IsNull <$ string' "isnull"
        -- NOTE: try is necessary because "[expr] NOT" might refer to unary
        -- postfix operators (NOTNULL or NOT NULL) but it could also be the
        -- start of a binary "[expr] NOT LIKE [expr]" or "[expr] NOT IN [expr]".
        , try (NotNull <$ string' "not" <* space <* string' "null")
        ]

    binary :: Parser (Expression -> Expression -> Expression)
    binary = choice
      [ Equals <$ string "==" <* space
      , Equals <$ string "=" <* space
      , NotEquals <$ string "<>" <* space
      , NotEquals <$ string "!=" <* space
      , Glob <$ string' "glob" <* space
      , Regexp <$ string' "regexp" <* space
      , Match <$ string' "match" <* space
      , Like <$ string' "like" <* space
      , string' "not" *> space1 *> binaryNot
      ]

    binaryNot :: Parser (Expression -> Expression -> Expression)
    binaryNot = choice
      [ NotGlob <$ string' "glob" <* space
      , NotRegexp <$ string' "regexp" <* space
      , NotMatch <$ string' "match" <* space
      , NotLike <$ string' "like" <* space
      ]

