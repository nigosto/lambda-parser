{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Transformers where

import Data.Char (chr, ord)
import Data.List (elemIndex, union)
import Data.Map (Map, empty, findMin, foldrWithKey, insert, keys, lookup, member, size)
import Prelude hiding (lookup)
import Utils.Variables (freeVariables, applicativeFreeVariables)
import Terms (Term (..), NamelessTerm (..), ApplicativeTerm (..), Combinator (..))
import Data.Bifunctor

type NamingContext = Map String Int
type BoundVariables = [String]
type VariableIndex = Int

getVariableIdentifier :: String -> Char
getVariableIdentifier = head

toNamelessWithContext :: Term -> Int -> BoundVariables -> NamingContext -> (NamelessTerm, NamingContext)
toNamelessWithContext (Variable var) depth bv context = case var `elemIndex` bv of
  Nothing -> case lookup var context of
    Nothing ->
      let index = size context
       in (NamelessVariable (index + depth), insert var index context)
    Just index -> (NamelessVariable (index + depth), context)
  Just index -> (NamelessVariable index, context)
toNamelessWithContext (Application lhs rhs) depth bv context =
  let namelessLhs = toNamelessWithContext lhs depth bv context
      namelessRhs = toNamelessWithContext rhs depth bv (snd namelessLhs)
   in first (NamelessApplication (fst namelessLhs)) namelessRhs
toNamelessWithContext (Abstraction argument body) depth bv context =
  let namelessBody = toNamelessWithContext body (depth + 1) (insertBoundVariable argument bv) context
   in first NamelessAbstraction namelessBody
  where
    insertBoundVariable var bv = if var `elem` bv then bv else var : bv

toNameless :: Term -> NamingContext -> (NamelessTerm, NamingContext)
toNameless term = toNamelessWithContext term 0 []

toNamedWithContext :: NamelessTerm -> BoundVariables -> NamingContext -> Term
toNamedWithContext (NamelessVariable var) bv context
  | var >= length bv = Variable $ findKey (var - length bv) context
  | otherwise = Variable (bv !! var)
toNamedWithContext (NamelessApplication lhs rhs) bv context =
  Application (toNamedWithContext lhs bv context) (toNamedWithContext rhs bv context)
toNamedWithContext (NamelessAbstraction body) bv context =
  let updatedBoundVariables = extendBoundVariables bv
   in Abstraction (head updatedBoundVariables) (toNamedWithContext body updatedBoundVariables context)
  where
    extendBoundVariables :: BoundVariables -> BoundVariables
    extendBoundVariables [] = if "a" `member` context then [nextVariable "a" 0] else ["a0"]
    extendBoundVariables bv@(x : _) = nextVariable x (length bv) : bv

toNamed :: NamelessTerm -> NamingContext -> Term
toNamed term = toNamedWithContext term []

findKey :: (Eq v) => v -> Map k v -> k
findKey value map = foldrWithKey (\k v res -> if v == value then k else res) (fst $ findMin map) map

nextVariable :: String -> VariableIndex -> String
nextVariable var index = getVariableIdentifier var : show index

toApplicative :: Term -> ApplicativeTerm
toApplicative (Variable var) = ApplicativeVariable var
toApplicative (Application lhs rhs) = ApplicativeApplication (toApplicative lhs) $ toApplicative rhs
toApplicative (Abstraction argument body)
  | body == Variable argument =
    ApplicativeApplication (ApplicativeApplication (ApplicativeCombinator SCombinator) $
                                                    ApplicativeCombinator KCombinator) $
                            ApplicativeCombinator KCombinator
  | argument `notElem` freeVariables body = ApplicativeApplication (ApplicativeCombinator KCombinator) $ toApplicative body
  | otherwise = case body of
    Application lhs rhs ->
      ApplicativeApplication (ApplicativeApplication (ApplicativeCombinator SCombinator) $
                                                      toApplicative (Abstraction argument lhs)) $
                              toApplicative (Abstraction argument rhs)
    _ -> toApplicativePartialAbstraction argument $ toApplicative body

-- This additional case is needed because the transformation to
-- applicative term is not deterministic. The following function
-- is used when the transformation has begun from the inside and
-- is currently backtracking with the incomplete term
toApplicativePartialAbstraction :: String -> ApplicativeTerm -> ApplicativeTerm
toApplicativePartialAbstraction argument term
  | term == ApplicativeVariable argument =
    ApplicativeApplication (ApplicativeApplication (ApplicativeCombinator SCombinator) $
                                                    ApplicativeCombinator KCombinator) $
                            ApplicativeCombinator KCombinator
  | argument `notElem` applicativeFreeVariables term = ApplicativeApplication (ApplicativeCombinator KCombinator) term
toApplicativePartialAbstraction argument (ApplicativeApplication lhs rhs) =
  ApplicativeApplication (ApplicativeApplication (ApplicativeCombinator SCombinator) $
                                                  toApplicativePartialAbstraction argument lhs) $
                          toApplicativePartialAbstraction argument rhs