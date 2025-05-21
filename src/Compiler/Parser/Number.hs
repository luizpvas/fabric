module Compiler.Parser.Number (Number(..), parser) where


import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Compiler.Parser.Error as Error


type Parser = Parsec Error.Error String


data Number
  = Int Int
  | Hex Int
  | Float Float
  deriving (Show, Eq)


parser :: Parser Number
parser =
  label "number" $
    choice
      [ toHex <$ string' "0x" <*> hexDigits
      , toFloatDecimalOnly <$> decimalDigits <*> (optional exponentDigits)
      , toIntOrFloat <$> digits <*> (optional decimalDigits) <*> (optional exponentDigits)
      ]


hexDigits :: Parser String
hexDigits =
  (:) <$> hexDigitChar <*> many underscoreSeparatedHexDigit
  where
    underscoreSeparatedHexDigit = choice [ id <$ char '_' <*> hexDigitChar, hexDigitChar ]


digits :: Parser String
digits =
  (:) <$> digitChar <*> many underscoreSeparatedDigit
  where
    underscoreSeparatedDigit = choice [ id <$ char '_' <*> digitChar, digitChar ]


decimalDigits :: Parser String
decimalDigits =
  id <$ char '.' <*> digits


exponentDigits :: Parser String
exponentDigits =
  id <$ char' 'e' <*> choice
    [ (:) <$> char '-' <*> digits
    , id <$ char '+' <*> digits
    , digits
    ]


toHex :: String -> Number
toHex hexDigits =
  Hex (read ("0x" ++ hexDigits))


toIntOrFloat :: String -> Maybe String -> Maybe String -> Number
toIntOrFloat digits_ Nothing Nothing =
  Int (read digits_)
toIntOrFloat digits_ Nothing (Just exponentDigits_) =
  Float (read (digits_ ++ "e" ++ exponentDigits_))
toIntOrFloat digits_ (Just decimalDigits_) Nothing = 
  Float (read (digits_ ++ "." ++ decimalDigits_))
toIntOrFloat digits_ (Just decimalDigits_) (Just exponentDigits_) = 
  Float (read (digits_ ++ "." ++ decimalDigits_ ++ "e" ++ exponentDigits_))


toFloatDecimalOnly :: String -> Maybe String -> Number
toFloatDecimalOnly decimalDigits_ Nothing =
  Float (read ("0." ++ decimalDigits_))
toFloatDecimalOnly decimalDigits_ (Just exponentDigits_) =
  Float (read ("0." ++ decimalDigits_ ++ "e" ++ exponentDigits_))
