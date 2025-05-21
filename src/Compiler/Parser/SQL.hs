module Compiler.Parser.SQL
  ( expression
  , expressionList
  ) where


-- Here are some interesting queries I found while writing this parser:
--
-- SELECT +++++++++++1
-- SELECT ~~~~~~~~~~~1
-- SELECT 'HELLO ' COLLATE RTRIM = 'HELLO'
-- SELECT NOT NULL NOTNULL IS NOT NULL


import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Compiler.Parser.SQL.AST
import qualified Compiler.Parser.Error as Error
import qualified Compiler.Parser.Name as Name
import qualified Compiler.Parser.Number as Number
import qualified Compiler.Parser.String as String


-- PARSER


type Parser = Parsec Error.Error String


-- EXPRESSION LIST


expressionList :: Parser Expression
expressionList = do
  first  <- expression
  others <- many (id <$ char ',' <* space <*> expression)
  return $ (ExpressionList (first : others))


-- EXPRESSION


expression :: Parser Expression
expression = expression11


expression0 :: Parser Expression
expression0 =
  choice
    [ literalNumber
    , literalString
    , literalBlob
    , literalNull
    , literalTrue
    , literalFalse
    , literalCurrent
    , columnName
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
  fmap LiteralString String.singleQuoted


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


columnName :: Parser Expression
columnName = do
  name1 <- name
  name2 <- (optional . try) (id <$ char' '.' <*> name)
  name3 <- (optional . try) (id <$ char' '.' <*> name)
  return $
    case (name2, name3) of
      ((Just n2), (Just n3)) -> SchemaTableColumnName name1 n2 n3
      ((Just n2), _)         -> TableColumnName name1 n2
      (_, _)                 -> ColumnName name1
  where
    name :: Parser String
    name = String.doubleQuoted <|> Name.variable


expression1 :: Parser Expression
expression1 =
  choice
    [ BitwiseNot <$ char '~' <*> expression1 <* space
    , Plus       <$ char '+' <*> expression1 <* space
    , Minus      <$ char '-' <*> expression1 <* space
    , expression0 <* space
    ]


expression2 :: Parser Expression
expression2 = do
  (\e f -> f e) <$> expression1 <*> (collate <|> pure id)
  where
    collate :: Parser (Expression -> Expression)
    collate =
      Collate <$ string' "collate" <* space1 <*> Name.variable <* space


expression3 :: Parser Expression
expression3 = do
  left  <- expression2
  pairs <- many ((,) <$> operator <*> expression2)
  return $ foldl (\l (op, r) -> op l r) left pairs
  where
    operator :: Parser (Expression -> Expression -> Expression)
    operator = choice
      [ StringConcatenation    <$ string "||"  <* space
      , JsonExtractDoubleArrow <$ string "->>" <* space
      , JsonExtractSingleArrow <$ string "->"  <* space
      ]


expression4 :: Parser Expression
expression4 = do
  left <- expression3
  pairs <- many ((,) <$> operator <*> expression3)
  return $ foldl (\l (op, r) -> op l r) left pairs
  where
    operator :: Parser (Expression -> Expression -> Expression)
    operator = choice
      [ Multiplication <$ string "*" <* space
      , Division       <$ string "/" <* space
      , Modulus        <$ string "%" <* space
      ]


expression5 :: Parser Expression
expression5 = do
  left <- expression4
  pairs <- many ((,) <$> operator <*> (expression4 <|> Error.binaryOperatorMissingRightExpression "+"))
  return $ foldl (\l (op, r) -> op l r) left pairs
  where
    operator :: Parser (Expression -> Expression -> Expression)
    operator = choice
      [ Sum         <$ string "+" <* space
      , Subtraction <$ string "-" <* space
      ]


expression6 :: Parser Expression
expression6 = do
  left <- expression5
  pairs <- many ((,) <$> operator <*> expression5)
  return $ foldl (\l (op, r) -> op l r) left pairs
  where
    operator :: Parser (Expression -> Expression -> Expression)
    operator = choice
      [ BitwiseAnd        <$ string "&"  <* space
      , BitwiseOr         <$ string "|"  <* space
      , BitwiseShiftLeft  <$ string "<<" <* space
      , BitwiseShiftRight <$ string ">>" <* space
      ]


expression7 :: Parser Expression
expression7 = do
  left <- expression6
  pairs <- many ((,) <$> operator <*> expression6)
  return $ foldl (\l (op, r) -> op l r) left pairs
  where
    operator :: Parser (Expression -> Expression -> Expression)
    operator = choice
      [ LessThanOrEqualTo <$ string "<=" <* space
      , GreaterThanOrEqualTo <$ string ">=" <* space
      , try (LessThan <$ string "<" <* notFollowedBy (char '>') <* space)
      , GreaterThan <$ string ">" <* space
      ]


data Expression8
  = NextUnary (Expression -> Expression)
  | NextBinary (Expression -> Expression -> Expression) Expression
  | NextBinaryWithEscape (Expression -> Expression -> EscapeClause -> Expression) Expression EscapeClause
  | NextTernary (Expression -> Expression -> Expression -> Expression) Expression Expression


expression8 :: Parser Expression
expression8 = do
  left <- expression7
  nexts <- many next
  return $ foldl eval left nexts
  where
    eval :: Expression -> Expression8 -> Expression
    eval left next =
      case next of
        NextUnary toExpr -> toExpr left
        NextBinary toExpr right -> toExpr left right
        NextBinaryWithEscape toExpr right escape -> toExpr left right escape
        NextTernary toExpr middle right -> toExpr left middle right

    next :: Parser Expression8
    next =
      choice
        [ NextUnary <$> unary
        , NextBinaryWithEscape <$> like <*> expression7 <*> escape
        , try (NextBinaryWithEscape <$> notLike <*> expression7 <*> escape)
        , NextTernary <$> between <*> expression7 <* string' "and" <* space1 <*> expression7
        , try (NextTernary <$> notBetween <*> expression7 <* string' "and" <* space1 <*> expression7)
        , NextBinary <$> binary <*> expression7
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

    like :: Parser (Expression -> Expression -> EscapeClause -> Expression)
    like = Like <$ string' "like" <* space1

    notLike :: Parser (Expression -> Expression -> EscapeClause -> Expression)
    notLike = NotLike <$ string' "not" <* space1 <* string' "like" <* space1

    escape :: Parser EscapeClause
    escape =
      choice
        [ Escape <$ string' "escape" <* space1 <*> expression7
        , pure NoEscape
        ]

    between :: Parser (Expression -> Expression -> Expression -> Expression)
    between = Between <$ string' "between" <* space1

    notBetween :: Parser (Expression -> Expression -> Expression -> Expression)
    notBetween = NotBetween <$ string' "not" <* space1 <* string' "between" <* space1

    binary :: Parser (Expression -> Expression -> Expression)
    binary = choice
      [ Equals <$ string "==" <* space
      , Equals <$ string "=" <* space
      , NotEquals <$ string "<>" <* space
      , NotEquals <$ string "!=" <* space
      , Glob <$ string' "glob" <* space
      , Regexp <$ string' "regexp" <* space
      , Match <$ string' "match" <* space
      , string' "is" *> space1 *> binaryIs
      , string' "not" *> space1 *> binaryNot
      ]

    binaryIs :: Parser (Expression -> Expression -> Expression)
    binaryIs = choice
      [ string' "not" *> space1 *> binaryIsNot
      , IsDistinctFrom <$ string' "distinct" <* space1 <* string' "from" <* space1
      , pure Is
      ]

    binaryIsNot :: Parser (Expression -> Expression -> Expression)
    binaryIsNot = choice
      [ IsNotDistinctFrom <$ string' "distinct" <* space1 <* string' "from" <* space1
      , pure IsNot
      ]

    binaryNot :: Parser (Expression -> Expression -> Expression)
    binaryNot = choice
      [ NotGlob <$ string' "glob" <* space
      , NotRegexp <$ string' "regexp" <* space
      , NotMatch <$ string' "match" <* space
      ]


expression9 :: Parser Expression
expression9 = do
  nots <- many (string' "not" <* space1)
  expr <- expression8
  return $ foldl (\expr _ -> Not expr) expr nots


expression10 :: Parser Expression
expression10 = do
  left <- expression9
  pairs <- many ((,) <$> string' "and" <* space1 <*> expression9)
  return $ foldl (\left (_, right) -> And left right) left pairs


expression11 :: Parser Expression
expression11 = do
  left <- expression10
  pairs <- many ((,) <$> string' "or" <* space1 <*> expression10)
  return $ foldl (\left (_, right) -> Or left right) left pairs
