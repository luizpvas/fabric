module Compiler.Parser.Number (Number(..), parser) where


import Data.Void
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char


data Number
  = Int Int
  | Hex Int
  | Float Int
  deriving (Show, Eq)

data Sign
  = Positive
  | Negative
  deriving (Show, Eq)

parser :: Parsec Void Text Number
parser =
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
           in Int (toSign (read intPart))
        Just decimalPart ->
          let toSign = if sign == Negative then negate else id
           in Float (toSign (read (intPart ++ "." ++ decimalPart)))
