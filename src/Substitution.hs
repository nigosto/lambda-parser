module Substitution where

import Parser (Token (Application, Variable, Abstraction))
import Data.Char (chr, ord)

freeVariables :: Token -> [Char]
freeVariables (Variable x) = [x]
freeVariables (Application lhs rhs) = freeVariables lhs ++ freeVariables rhs
freeVariables (Abstraction argument body) = filter (/= argument) $ freeVariables body

chooseUniqueVariable :: Token -> Token -> Char
chooseUniqueVariable first second = let vars = freeVariables first ++ freeVariables second
  in chooseVariable vars 'a'
    where chooseVariable :: [Char] -> Char -> Char
          chooseVariable vars current
            | current `notElem` vars = current
            | otherwise = chooseVariable vars $ chr $ ord current + 1

substitute :: Token -> Char -> Token -> Token
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