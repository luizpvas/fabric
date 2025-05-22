module Compiler.Parser.Error
  ( Error
  , binaryOperatorMissingRightExpression
  ) where


import Text.Megaparsec


data Error
  = BinaryOperatorMissingRightExpression String
  deriving (Show, Eq, Ord)


instance ShowErrorComponent Error where
  showErrorComponent (BinaryOperatorMissingRightExpression operator) =
    "I just saw a plus sign, so I was expecting to see an expression next.\n" ++
    "Something like 42 or 1000 that makes sense with a + sign."


binaryOperatorMissingRightExpression :: String -> Parsec Error String a
binaryOperatorMissingRightExpression =
  customFailure . BinaryOperatorMissingRightExpression
