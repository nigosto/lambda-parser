{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
module Transformer where

import Prelude hiding (lookup)
import Parser (Term (Variable, Application, Abstraction))
import Data.Map (Map, member, lookup, insert, size)
import Data.List (elemIndex, union)

data NamelessTerm
  = NamelessVariable Int
  | NamelessApplication NamelessTerm NamelessTerm
  | NamelessAbstraction Int NamelessTerm
  deriving (Show)

type NamingContext = Map Char Int
type BoundVariable = [Char]

toNameless :: Term -> Int -> BoundVariable -> NamingContext -> (NamelessTerm, NamingContext)
toNameless (Variable var) depth bv context = case var `elemIndex` bv of
    Nothing -> case lookup var context of
        Nothing -> let index = size context
                   in (NamelessVariable (index + depth), insert var index context)
        Just index -> (NamelessVariable (index + depth), context)
    Just index -> (NamelessVariable index, context)

toNameless (Application lhs rhs) depth bv context =
    let namelessLhs = toNameless lhs depth bv context
        namelessRhs = toNameless rhs depth bv (snd namelessLhs)
    in (NamelessApplication (fst namelessLhs) (fst namelessRhs), snd namelessRhs)

toNameless (Abstraction argument body) depth bv context = 
    let namelessBody = toNameless body (depth + 1) (insertBoundVariable argument bv) context
    in (NamelessAbstraction depth (fst namelessBody), snd namelessBody)
    where insertBoundVariable var bv = if var `elem` bv then bv else var:bv

-- a = Abstraction 'y' (Application (Variable 'x') (Application (Variable 'y') (Abstraction 'x' (Variable 'x'))))
-- /x -> xy => Abstraction x (Application (Variable x) (Variable y))
-- /01 => NamelessAbstraction 1 (NamelessApplication (Variable 0) (Variable 1))

-- /y -> xy/x -> x => /10/0
-- (NamelessAbstraction (NamelessApplication (NamelessVariable 1) (NamelessApplication (NamelessVariable 0) (NamelessAbstraction (NamelessVariable 1))))