module Substitution.Named where

import Terms (Term (Application, Variable, Abstraction))
import Data.Char (chr, ord)
import Utils.Variables (freeVariables)

chooseUniqueVariable :: Term -> Term -> String
chooseUniqueVariable first second = let vars = freeVariables first ++ freeVariables second
  in chooseVariable vars "a"
    where chooseVariable :: [String] -> String -> String
          chooseVariable vars current
            | current `notElem` vars = current
            -- TODO: fix
            | otherwise = chooseVariable vars $ show $ chr $ ord (head current) + 1

substitute :: Term -> String -> Term -> Term
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