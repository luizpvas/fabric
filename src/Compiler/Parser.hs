module Compiler.Parser where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text
