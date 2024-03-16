{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
module Transformer where

import Prelude hiding (lookup)
import Parser (Term (Variable, Application, Abstraction))
import Data.Map (Map, member, lookup, insert, size, foldrWithKey, findMin, keys, empty)
import Data.List (elemIndex, union)
import Data.Char (chr, ord)

data NamelessTerm
  = NamelessVariable Int
  | NamelessApplication NamelessTerm NamelessTerm
  | NamelessAbstraction NamelessTerm
  deriving (Show)

type NamingContext = Map Char Int
type BoundVariables = [Char]

toNamelessWithContext :: Term -> Int -> BoundVariables -> NamingContext -> (NamelessTerm, NamingContext)
toNamelessWithContext (Variable var) depth bv context = case var `elemIndex` bv of
    Nothing -> case lookup var context of
        Nothing -> let index = size context
                   in (NamelessVariable (index + depth), insert var index context)
        Just index -> (NamelessVariable (index + depth), context)
    Just index -> (NamelessVariable index, context)

toNamelessWithContext (Application lhs rhs) depth bv context =
    let namelessLhs = toNamelessWithContext lhs depth bv context
        namelessRhs = toNamelessWithContext rhs depth bv (snd namelessLhs)
    in (NamelessApplication (fst namelessLhs) (fst namelessRhs), snd namelessRhs)

toNamelessWithContext (Abstraction argument body) depth bv context =
    let namelessBody = toNamelessWithContext body (depth + 1) (insertBoundVariable argument bv) context
    in (NamelessAbstraction (fst namelessBody), snd namelessBody)
    where insertBoundVariable var bv = if var `elem` bv then bv else var:bv

toNameless :: Term -> NamingContext -> (NamelessTerm, NamingContext)
toNameless term = toNamelessWithContext term 0 []

toNamedWithContext :: NamelessTerm  -> BoundVariables -> NamingContext -> Term
toNamedWithContext (NamelessVariable var) bv context
    | var >= length bv = Variable $ findKey (var - length bv) context
    | otherwise = Variable (bv !! var)
toNamedWithContext (NamelessApplication lhs rhs) bv context =
    Application (toNamedWithContext lhs bv context) (toNamedWithContext rhs bv context)
toNamedWithContext (NamelessAbstraction body) bv context = let updatedBoundVariables = extendBoundVariables bv
    in Abstraction (head updatedBoundVariables) (toNamedWithContext body updatedBoundVariables context)
    where extendBoundVariables :: BoundVariables -> BoundVariables
          extendBoundVariables [] = if 'a' `member` context then [nextVariable 'a' context] else "a"
          extendBoundVariables bv@(x:_) = nextVariable x context:bv

toNamed :: NamelessTerm -> NamingContext -> Term
toNamed term = toNamedWithContext term []

findKey :: Eq v => v -> Map k v -> k
findKey value map = foldrWithKey (\k v res -> if v == value then k else res) (fst $ findMin map) map

nextVariable :: Char -> NamingContext -> Char
nextVariable var context = let next = chr $ ord var + 1
    in if next `member` context then nextVariable next context else next
