module Compiler.Parser.Name (variable) where


import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char


variable :: Parsec Void String String
variable = identifier


identifier :: Parsec Void String String
identifier =
  (:) <$> (letterChar <|> char '_') <*> many (letterChar <|> char '_' <|> digitChar)
