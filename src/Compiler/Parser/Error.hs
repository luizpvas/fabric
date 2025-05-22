module Compiler.Parser.Error
  ( Error
  , sumMissingRightExpression
  , subtractionMissingRightExpression
  ) where


import Text.Megaparsec


data Error
  = SumMissingRightExpression
  | SubtractionMissingRightExpression
  deriving (Show, Eq, Ord)


instance ShowErrorComponent Error where
  showErrorComponent (SumMissingRightExpression) =
    "I just saw a plus sign, so I was expecting to see an expression next.\n" ++
    "Something like 42 or 1000 that makes sense with a + sign."

  showErrorComponent (SubtractionMissingRightExpression) =
    "I just saw a minus sign, so I was expecting to see an expressio next.\n" ++
    "Something like 42 or 100 that makes sense with a - sign."


sumMissingRightExpression :: Parsec Error String a
sumMissingRightExpression = customFailure SumMissingRightExpression

subtractionMissingRightExpression :: Parsec Error String a
subtractionMissingRightExpression = customFailure SubtractionMissingRightExpression
