module Compiler.SQL.Parser where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char


-- PARSER


type Parser = Parsec Void Text


-- EXPR


data Expr
  = LiteralInt Int
  | LiteralFloat Float
  | LiteralString Text
  | LiteralNull
  | LiteralTrue
  | LiteralFalse
  | LiteralCurrentTime
  | LiteralCurrentDate
  | LiteralCurrentTimestamp
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
  choice
    [ toLiteral Negative <$ char '-' <*> digits <*> decimalDigits
    , toLiteral Positive <$> digits <*> decimalDigits
    ]
  where
    digits = some digitChar
    decimalDigits = choice [Just <$ char '.' <*> digits, pure Nothing]
    toLiteral sign intPart maybeDecimalPart =
      case maybeDecimalPart of
        Nothing ->
          let toSign = if sign == Negative then negate else id
           in LiteralInt (toSign (read intPart))
        Just decimalPart ->
          let toSign = if sign == Negative then negate else id
           in LiteralFloat (toSign (read (intPart ++ "." ++ decimalPart)))
