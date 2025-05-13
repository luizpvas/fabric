module Compiler.Database.Relational.Column where

data Column = Column
  { _name :: String,
    _type :: ColumnType,
    _constraints :: [ColumnConstraint]
  }

data ColumnType
  = SignedInt Int
  | UnsignedInt Int
  | Decimal Int Int
  | Float
  | Double
  | Varchar Int
  | Text
  | Date
  | Time
  | DateTimeWithoutTimeZone
  | DateTimeWithTimeZone
  | Json
  | Jsonb

data ColumnConstraint
  = PrimaryKey
  | NotNull
  | AutoIncrement