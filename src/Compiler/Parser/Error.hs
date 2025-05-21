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
    "binary operator " ++ operator ++ " needs a right expression"


binaryOperatorMissingRightExpression :: String -> Parsec Error String a
binaryOperatorMissingRightExpression =
  customFailure . BinaryOperatorMissingRightExpression
