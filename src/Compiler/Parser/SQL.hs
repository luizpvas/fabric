module Compiler.Parser.SQL
  ( expression
  , expressionList
  , tableNameOrTableFunction
  ) where


-- Here are some interesting queries I found while writing this parser:
--
-- SELECT +++++++++++1
-- SELECT ~~~~~~~~~~~1
-- SELECT 'HELLO ' COLLATE RTRIM = 'HELLO'
-- SELECT NOT NULL NOTNULL IS NOT NULL


import Data.Void
import Compiler.Parser.SQL.AST
import Control.Applicative ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Compiler.Parser.Error as Error
import qualified Compiler.Parser.Name as Name
import qualified Compiler.Parser.Number as Number
import qualified Compiler.Parser.String as String


-- PARSER


type Parser = P.Parsec Error.Error String


-- EXPRESSION LIST


expressionList :: Parser [Expression]
expressionList =
  P.choice [ nonEmptyList, emptyList ]
  where
    nonEmptyList = (:) <$> expression <*> P.many (id <$ C.char ',' <* C.space <*> expression)
    emptyList = pure []



-- EXPRESSION


expression :: Parser Expression
expression = expression11


expression0 :: Parser Expression
expression0 =
  P.choice
    [ literalNumber
    , literalString
    , literalBlob
    , literalNull
    , literalTrue
    , literalFalse
    , literalCurrent
    , columnName
    , fmap Parenthesized (P.between (C.char '(') (C.char ')') expression)
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
  LiteralBlob <$ C.char' 'x' <* C.char '\'' <*> P.many C.hexDigitChar <* C.char '\''


literalNull :: Parser Expression
literalNull =
  LiteralNull <$ C.string' "null"


literalTrue :: Parser Expression
literalTrue =
  LiteralTrue <$ C.string' "true"


literalFalse :: Parser Expression
literalFalse =
  LiteralFalse <$ C.string' "false"


literalCurrent :: Parser Expression
literalCurrent =
  C.string' "current_" *> trailing
  where
    trailing :: Parser Expression
    trailing = P.choice
      [ LiteralCurrentDate      <$ C.string' "date"
      , LiteralCurrentTimestamp <$ C.string' "timestamp"
      , LiteralCurrentTime      <$ C.string' "time"
      ]


columnName :: Parser Expression
columnName = do
  name1 <- name
  name2 <- (P.optional . P.try) (id <$ C.char' '.' <*> name)
  name3 <- (P.optional . P.try) (id <$ C.char' '.' <*> name)
  return $
    case (name2, name3) of
      ((Just n2), (Just n3)) -> SchemaTableColumnName name1 n2 n3
      ((Just n2), _)         -> TableColumnName name1 n2
      (_, _)                 -> ColumnName name1
  where
    name :: Parser String
    name = String.doubleQuoted <|> Name.variable


tableNameOrTableFunction :: Parser Expression
tableNameOrTableFunction = do
  name1 <- name
  name2 <- (P.optional . P.try) (id <$ C.char '.' <*> name)
  arguments <- (P.optional . P.try) (P.between (C.char '(') (C.char ')') expressionList)
  return $
    case (name2, arguments) of
      ((Just n2), (Just args)) -> SchemaTableFunction name1 n2 args
      ((Just n2), Nothing)     -> SchemaTableName name1 n2
      (Nothing, (Just args))   -> TableFunction name1 args
      (Nothing, Nothing)       -> TableName name1
  where
    name :: Parser String
    name = String.doubleQuoted <|> Name.variable


expression1 :: Parser Expression
expression1 =
  P.choice
    [ BitwiseNot <$ C.char '~' <*> expression1 <* C.space
    , Plus <$ C.char '+' <*> expression1 <* C.space
    , Minus <$ C.char '-' <*> expression1 <* C.space
    , expression0 <* C.space
    ]


expression2 :: Parser Expression
expression2 = do
  (\e f -> f e) <$> expression1 <*> (collate <|> pure id)
  where
    collate :: Parser (Expression -> Expression)
    collate =
      Collate <$ C.string' "collate" <* C.space1 <*> Name.variable <* C.space


expression3 :: Parser Expression
expression3 = do
  left  <- expression2
  pairs <- P.many ((,) <$> operator <*> expression2)
  return $ foldl (\l (op, r) -> op l r) left pairs
  where
    operator :: Parser (Expression -> Expression -> Expression)
    operator = P.choice
      [ StringConcatenation    <$ C.string "||"  <* C.space
      , JsonExtractDoubleArrow <$ C.string "->>" <* C.space
      , JsonExtractSingleArrow <$ C.string "->"  <* C.space
      ]


expression4 :: Parser Expression
expression4 = do
  left <- expression3
  pairs <- P.many ((,) <$> operator <*> expression3)
  return $ foldl (\l (op, r) -> op l r) left pairs
  where
    operator :: Parser (Expression -> Expression -> Expression)
    operator = P.choice
      [ Multiplication <$ C.string "*" <* C.space
      , Division       <$ C.string "/" <* C.space
      , Modulus        <$ C.string "%" <* C.space
      ]


data Expression5
  = NextSum Expression
  | NextSubtraction Expression


expression5 :: Parser Expression
expression5 = do
  left <- expression4
  nexts <- P.many next
  return $ foldl solve left nexts
  where
    solve :: Expression -> Expression5 -> Expression
    solve left (NextSum right) = Sum left right
    solve left (NextSubtraction right) = Subtraction left right

    next :: Parser Expression5
    next = P.choice
      [ NextSum         <$ C.string "+" <* C.space <*> (expression4 <|> Error.sumMissingRightExpression)
      , NextSubtraction <$ C.string "-" <* C.space <*> (expression4 <|> Error.subtractionMissingRightExpression)
      ]


expression6 :: Parser Expression
expression6 = do
  left <- expression5
  pairs <- P.many ((,) <$> operator <*> expression5)
  return $ foldl (\l (op, r) -> op l r) left pairs
  where
    operator :: Parser (Expression -> Expression -> Expression)
    operator = P.choice
      [ BitwiseAnd        <$ C.string "&"  <* C.space
      , BitwiseOr         <$ C.string "|"  <* C.space
      , BitwiseShiftLeft  <$ C.string "<<" <* C.space
      , BitwiseShiftRight <$ C.string ">>" <* C.space
      ]


expression7 :: Parser Expression
expression7 = do
  left <- expression6
  pairs <- P.many ((,) <$> operator <*> expression6)
  return $ foldl (\l (op, r) -> op l r) left pairs
  where
    operator :: Parser (Expression -> Expression -> Expression)
    operator = P.choice
      [ LessThanOrEqualTo <$ C.string "<=" <* C.space
      , GreaterThanOrEqualTo <$ C.string ">=" <* C.space
      , P.try (LessThan <$ C.string "<" <* P.notFollowedBy (C.char '>') <* C.space)
      , GreaterThan <$ C.string ">" <* C.space
      ]


data Expression8
  = NextUnary (Expression -> Expression)
  | NextBinary (Expression -> Expression -> Expression) Expression
  | NextBinaryWithEscape (Expression -> Expression -> EscapeClause -> Expression) Expression EscapeClause
  | NextIn (Expression -> Expression -> Expression) Expression
  | NextTernary (Expression -> Expression -> Expression -> Expression) Expression Expression


expression8 :: Parser Expression
expression8 = do
  left <- expression7
  nexts <- P.many next
  return $ foldl eval left nexts
  where
    eval :: Expression -> Expression8 -> Expression
    eval left next =
      case next of
        NextUnary toExpr -> toExpr left
        NextBinary toExpr right -> toExpr left right
        NextBinaryWithEscape toExpr right escape -> toExpr left right escape
        NextTernary toExpr middle right -> toExpr left middle right
        NextIn toExpr right -> toExpr left right

    next :: Parser Expression8
    next =
      P.choice
        [ NextUnary <$> unary
        , NextBinaryWithEscape <$> like <*> expression7 <*> escape
        , P.try (NextBinaryWithEscape <$> notLike <*> expression7 <*> escape)
        , NextTernary <$> between <*> expression7 <* C.string' "and" <* C.space1 <*> expression7
        , P.try (NextTernary <$> notBetween <*> expression7 <* C.string' "and" <* C.space1 <*> expression7)
        , NextIn <$> inOperator <*> inExpression
        , NextBinary <$> binary <*> expression7
        ]

    unary :: Parser (Expression -> Expression)
    unary =
      P.choice
        [ IsNull <$ C.string' "isnull"
        -- NOTE: try is necessary because "[expr] NOT" might refer to unary
        -- postfix operators (NOTNULL or NOT NULL) but it could also be the
        -- start of a binary "[expr] NOT LIKE [expr]" or "[expr] NOT IN [expr]".
        , P.try (NotNull <$ C.string' "not" <* C.space <* C.string' "null")
        ]

    like :: Parser (Expression -> Expression -> EscapeClause -> Expression)
    like = Like <$ C.string' "like" <* C.space1

    notLike :: Parser (Expression -> Expression -> EscapeClause -> Expression)
    notLike = NotLike <$ C.string' "not" <* C.space1 <* C.string' "like" <* C.space1

    escape :: Parser EscapeClause
    escape =
      P.choice
        [ Escape <$ C.string' "escape" <* C.space1 <*> expression7
        , pure NoEscape
        ]

    between :: Parser (Expression -> Expression -> Expression -> Expression)
    between = Between <$ C.string' "between" <* C.space1

    notBetween :: Parser (Expression -> Expression -> Expression -> Expression)
    notBetween = NotBetween <$ C.string' "not" <* C.space1 <* C.string' "between" <* C.space1

    inOperator :: Parser (Expression -> Expression -> Expression)
    inOperator = In <$ C.string' "in" <* C.space1

    inExpression :: Parser Expression
    inExpression =
      P.choice
        [ ExpressionList <$> P.between (C.char '(') (C.char ')') expressionList
        ]

    binary :: Parser (Expression -> Expression -> Expression)
    binary = P.choice
      [ Equals <$ C.string "==" <* C.space
      , Equals <$ C.string "=" <* C.space
      , NotEquals <$ C.string "<>" <* C.space
      , NotEquals <$ C.string "!=" <* C.space
      , Glob <$ C.string' "glob" <* C.space
      , Regexp <$ C.string' "regexp" <* C.space
      , Match <$ C.string' "match" <* C.space
      , C.string' "is" *> C.space1 *> binaryIs
      , C.string' "not" *> C.space1 *> binaryNot
      ]

    binaryIs :: Parser (Expression -> Expression -> Expression)
    binaryIs = P.choice
      [ C.string' "not" *> C.space1 *> binaryIsNot
      , IsDistinctFrom <$ C.string' "distinct" <* C.space1 <* C.string' "from" <* C.space1
      , pure Is
      ]

    binaryIsNot :: Parser (Expression -> Expression -> Expression)
    binaryIsNot = P.choice
      [ IsNotDistinctFrom <$ C.string' "distinct" <* C.space1 <* C.string' "from" <* C.space1
      , pure IsNot
      ]

    binaryNot :: Parser (Expression -> Expression -> Expression)
    binaryNot = P.choice
      [ NotGlob <$ C.string' "glob" <* C.space
      , NotRegexp <$ C.string' "regexp" <* C.space
      , NotMatch <$ C.string' "match" <* C.space
      ]


expression9 :: Parser Expression
expression9 = do
  nots <- P.many (C.string' "not" <* C.space1)
  expr <- expression8
  return $ foldl (\expr _ -> Not expr) expr nots


expression10 :: Parser Expression
expression10 = do
  left <- expression9
  pairs <- P.many ((,) <$> C.string' "and" <* C.space1 <*> expression9)
  return $ foldl (\left (_, right) -> And left right) left pairs


expression11 :: Parser Expression
expression11 = do
  left <- expression10
  pairs <- P.many ((,) <$> C.string' "or" <* C.space1 <*> expression10)
  return $ foldl (\left (_, right) -> Or left right) left pairs
