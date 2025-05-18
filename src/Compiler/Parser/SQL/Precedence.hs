module Compiler.Parser.SQL.Precedence (fix) where


import Compiler.Parser.SQL.AST


fix :: Expression -> Expression
fix (Operator o) = Operator (f o)
fix uninteresting = uninteresting


f :: Operator -> Operator
-- ~expr
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
-- +prefix
f (Plus (Operator (Collate n e)))                = Collate n (Operator (Plus e))
f (Plus (Operator (StringConcatenation l r)))    = StringConcatenation (Operator (Plus l)) r
f (Plus (Operator (JsonExtractSingleArrow l r))) = JsonExtractSingleArrow (Operator (Plus l)) r
f (Plus (Operator (JsonExtractDoubleArrow l r))) = JsonExtractDoubleArrow (Operator (Plus l)) r
f (Plus (Operator (Multiplication l r)))         = Multiplication (Operator (Plus l)) r
f (Plus (Operator (Division l r)))               = Division (Operator (Plus l)) r
f (Plus (Operator (Modulus l r)))                = Modulus (Operator (Plus l)) r
f (Plus (Operator (Sum l r)))                    = Sum (Operator (Plus l)) r
f (Plus (Operator (Subtraction l r)))            = Subtraction (Operator (Plus l)) r
f (Plus (Operator (BitwiseAnd l r)))             = BitwiseAnd (Operator (Plus l)) r
f (Plus (Operator (BitwiseOr l r)))              = BitwiseOr (Operator (Plus l)) r
f (Plus (Operator (BitwiseShiftLeft l r)))       = BitwiseShiftLeft (Operator (Plus l)) r
f (Plus (Operator (BitwiseShiftRight l r)))      = BitwiseShiftRight (Operator (Plus l)) r
f (Plus (Operator (LessThan l r)))               = LessThan (Operator (Plus l)) r
f (Plus (Operator (LessThanOrEqualTo l r)))      = LessThanOrEqualTo (Operator (Plus l)) r
f (Plus (Operator (GreaterThan l r)))            = GreaterThan (Operator (Plus l)) r
f (Plus (Operator (GreaterThanOrEqualTo l r)))   = GreaterThanOrEqualTo (Operator (Plus l)) r
f (Plus (Operator (Equals l r)))                 = Equals (Operator (Plus l)) r
f (Plus (Operator (NotEquals l r)))              = NotEquals (Operator (Plus l)) r
f (Plus (Operator (Is l r)))                     = Is (Operator (Plus l)) r
f (Plus (Operator (IsNot l r)))                  = IsNot (Operator (Plus l)) r
f (Plus (Operator (IsDistinctFrom l r)))         = IsDistinctFrom (Operator (Plus l)) r
f (Plus (Operator (IsNotDistinctFrom l r)))      = IsNotDistinctFrom (Operator (Plus l)) r
f (Plus (Operator (And l r)))                    = And (Operator (Plus l)) r
f (Plus (Operator (Or l r)))                     = Or (Operator (Plus l)) r
-- fallback
f operator = operator