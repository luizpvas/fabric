module Compiler.Parser.Name (Name(..), variable) where


import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char


data Name
  = Sole String
  | Qualified Name Name
  deriving (Show, Eq)


variable :: Parsec Void String Name
variable =
  Sole <$> identifier


identifier :: Parsec Void String String
identifier =
  (:) <$> (letterChar <|> char '_') <*> many (letterChar <|> char '_' <|> digitChar)
