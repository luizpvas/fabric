module Compiler.Parser.Name (variable) where


import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Compiler.Parser.Error as Error


variable :: Parsec Error.Error String String
variable = identifier


identifier :: Parsec Error.Error String String
identifier =
  (:) <$> (letterChar <|> char '_') <*> many (letterChar <|> char '_' <|> digitChar)
