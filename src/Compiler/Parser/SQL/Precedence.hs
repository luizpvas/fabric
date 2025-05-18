module Compiler.Parser.SQL.Precedence (fix) where


import Compiler.Parser.SQL.AST


fix :: Expression -> Expression
fix (Operator o) = Operator (f o)
fix uninteresting = uninteresting


f :: Operator -> Operator
f (BitwiseNot (Operator (Collate n e)))                = Collate n (Operator (BitwiseNot e))
f (BitwiseNot (Operator (StringConcatenation l r)))    = StringConcatenation (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (JsonExtractSingleArrow l r))) = JsonExtractSingleArrow (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (JsonExtractDoubleArrow l r))) = JsonExtractDoubleArrow (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (Multiplication l r)))         = Multiplication (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (Division l r)))               = Division (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (Modulus l r)))                = Modulus (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (Sum l r)))                    = Sum (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (Subtraction l r)))            = Subtraction (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (BitwiseAnd l r)))             = BitwiseAnd (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (BitwiseOr l r)))              = BitwiseOr (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (BitwiseShiftLeft l r)))       = BitwiseShiftLeft (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (BitwiseShiftRight l r)))      = BitwiseShiftRight (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (LessThan l r)))               = LessThan (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (LessThanOrEqualTo l r)))      = LessThanOrEqualTo (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (GreaterThan l r)))            = GreaterThan (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (GreaterThanOrEqualTo l r)))   = GreaterThanOrEqualTo (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (Equals l r)))                 = Equals (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (NotEquals l r)))              = NotEquals (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (Is l r)))                     = Is (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (IsNot l r)))                  = IsNot (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (IsDistinctFrom l r)))         = IsDistinctFrom (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (IsNotDistinctFrom l r)))      = IsNotDistinctFrom (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (And l r)))                    = And (Operator (BitwiseNot l)) r
f (BitwiseNot (Operator (Or l r)))                     = Or (Operator (BitwiseNot l)) r
f operator = operator