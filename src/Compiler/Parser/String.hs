module Compiler.Parser.String
  ( singleQuoted
  , doubleQuoted
  ) where


import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Compiler.Parser.Error as Error


singleQuoted :: Parsec Error.Error String String
singleQuoted =
  concat <$> between (char singleQuote) (char singleQuote) (many (try escaped <|> uninteresting))
  where
    escaped = (\a -> [a]) <$ char singleQuote <*> char singleQuote
    uninteresting = (\a -> [a]) <$> satisfy (\c -> c /= singleQuote)
    singleQuote = '\''


doubleQuoted :: Parsec Error.Error String String
doubleQuoted =
  concat <$> between (char doubleQuote) (char doubleQuote) (many (try escaped <|> uninteresting))
  where
    escaped = (\a -> [a]) <$ char escape <*> char doubleQuote
    uninteresting = (\a -> [a]) <$> satisfy (\c -> c /= doubleQuote)
    escape = '\\'
    doubleQuote   = '\"'
