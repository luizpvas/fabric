module Compiler.Parser.Number (Number(..), parser) where


import Data.Function ((&))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char


data Number
  = Int Int
  | Hex Int
  | Float Float
  deriving (Show, Eq)


parser :: Parsec Void String Number
parser =
  label "number" $
    choice
      [ negative <$ char '-' <*> parserHelper
      , positive <$ char '+' <*> parserHelper
      , parserHelper
      ]


parserHelper :: Parsec Void String Number
parserHelper =
  choice
    [ toHex <$ string' "0x" <*> hexDigits
    , toFloatDecimalOnly <$> decimalDigits
    , toIntOrFloat <$> digits <*> (optional decimalDigits)
    ]


hexDigits :: Parsec Void String String
hexDigits =
  (:) <$> hexDigitChar <*> many underscoreSeparatedHexDigit
  where
    underscoreSeparatedHexDigit = choice [ id <$ char '_' <*> hexDigitChar, hexDigitChar ]


digits :: Parsec Void String String
digits =
  (:) <$> digitChar <*> many underscoreSeparatedDigit
  where
    underscoreSeparatedDigit = choice [ id <$ char '_' <*> digitChar, digitChar ]
      

decimalDigits :: Parsec Void String String
decimalDigits =
  id <$ char '.' <*> digits


toHex :: String -> Number
toHex hexDigits =
  Hex (read ("0x" ++ hexDigits))


toIntOrFloat :: String -> Maybe String -> Number
toIntOrFloat intDigits Nothing = Int (read intDigits)
toIntOrFloat intDigits (Just decimalDigits) = Float (read (intDigits ++ "." ++ decimalDigits))


toFloatDecimalOnly :: String -> Number
toFloatDecimalOnly decimalDigits = Float (read ("0." ++ decimalDigits))


positive :: Number -> Number
positive = id


negative :: Number -> Number
negative (Int i)   = Int   $ negate i
negative (Hex h)   = Hex   $ negate h
negative (Float f) = Float $ negate f
