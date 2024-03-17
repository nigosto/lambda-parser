module Terms where

data Term =
  Variable Char |
  Application Term Term |
  Abstraction Char Term
  deriving (Show, Eq)

data NamelessTerm
  = NamelessVariable Int
  | NamelessApplication NamelessTerm NamelessTerm
  | NamelessAbstraction NamelessTerm
  deriving (Show)

data Combinator = KCombinator | SCombinator deriving (Eq)

data ApplicativeTerm
  = ApplicativeVariable Char
  | ApplicativeApplication ApplicativeTerm ApplicativeTerm
  | ApplicativeCombinator Combinator
  deriving (Eq)