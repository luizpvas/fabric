module Compiler.Parser.String (singleQuotedParser) where


import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char


singleQuotedParser :: Parsec Void String String
singleQuotedParser =
  concat <$> between (char singleQuote) (char singleQuote) (many (try escaped <|> uninteresting))
  where
    escaped       = (\a -> [a]) <$ char singleQuote <*> char singleQuote
    uninteresting = (\a -> [a]) <$> satisfy (\c -> c /= singleQuote)
    singleQuote   = '\''
