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
    "I just saw a + sign, so I was expecting to see an expression next.\n" ++
    "You're probably missing a number there!"

  showErrorComponent (SubtractionMissingRightExpression) =
    "I just saw a - sign, so I was expecting to see an expression next.\n" ++
    "You're probably missing a number there!"


sumMissingRightExpression :: Parsec Error String a
sumMissingRightExpression = customFailure SumMissingRightExpression

subtractionMissingRightExpression :: Parsec Error String a
subtractionMissingRightExpression = customFailure SubtractionMissingRightExpression
