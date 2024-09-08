module Terms where

data Term =
  Variable String |
  Application Term Term |
  Abstraction String Term
  deriving (Show, Eq)

data NamelessTerm
  = NamelessVariable Int
  | NamelessApplication NamelessTerm NamelessTerm
  | NamelessAbstraction NamelessTerm
  deriving (Show)

data Combinator = KCombinator | SCombinator deriving (Eq)

data ApplicativeTerm
  = ApplicativeVariable String
  | ApplicativeApplication ApplicativeTerm ApplicativeTerm
  | ApplicativeCombinator Combinator
  deriving (Eq)