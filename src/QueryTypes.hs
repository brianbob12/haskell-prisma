module QueryTypes where

data IntQuery = 
  IntNothing
  | IntEqual Int
  | IntNotEqual Int
  | IntGreaterThan Int
  | IntGreaterThanOrEqual Int
  | IntLessThan Int
  | IntLessThanOrEqual Int
  | IntBetween Int Int
  | IntIn [Int]
  | IntNot IntQuery
  | IntAnd IntQuery IntQuery
  | IntOr IntQuery IntQuery
  deriving (Show, Eq)

data StringQuery = 
  StringNothing
  | StringEqual String
  | StringNoCaseEqual String
  | StringNotEqual String
  | StringContains String
  | StringNotContains String
  | StringStartsWith String
  | StringEndsWith String
  | StringRegex String
  | StringIn [String]
  | StringNot StringQuery
  | StringAnd StringQuery StringQuery
  | StringOr StringQuery StringQuery
  deriving (Show, Eq)

