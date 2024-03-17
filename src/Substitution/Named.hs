module Substitution.Named where

import Terms (Term (Application, Variable, Abstraction))
import Data.Char (chr, ord)
import Utils.Variables (freeVariables)

chooseUniqueVariable :: Term -> Term -> Char
chooseUniqueVariable first second = let vars = freeVariables first ++ freeVariables second
  in chooseVariable vars 'a'
    where chooseVariable :: [Char] -> Char -> Char
          chooseVariable vars current
            | current `notElem` vars = current
            | otherwise = chooseVariable vars $ chr $ ord current + 1

substitute :: Term -> Char -> Term -> Term
substitute initial@(Variable var) x token
  | var == x = token
  | otherwise = initial

substitute (Application lhs rhs) x token = Application (substitute lhs x token) (substitute rhs x token)

substitute initial@(Abstraction argument body) x token
  | argument == x = initial
  | x `notElem` freeVariables body || 
    argument `notElem` freeVariables token = 
      Abstraction argument (substitute body x token)
  | otherwise = let var = chooseUniqueVariable body token
                in Abstraction var $ substitute (substitute body argument (Variable var)) x token 