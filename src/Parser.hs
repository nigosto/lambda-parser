module Parser (parseTerm) where

import Data.Char (isAlpha)
import Stack (Stack, peek, pop, emptyStack, push)
import Terms (Term (..))

takeWhileClosingBracket :: String -> Int-> String
takeWhileClosingBracket ('(':xs) count = '(':takeWhileClosingBracket xs (count + 1)
takeWhileClosingBracket (')':xs) 1 = ""
takeWhileClosingBracket (')':xs) count = ')':takeWhileClosingBracket xs (count - 1)
takeWhileClosingBracket (x:xs) count = x:takeWhileClosingBracket xs count

dropWhileClosingBracket :: String -> Int-> String
dropWhileClosingBracket ('(':xs) count = dropWhileClosingBracket xs (count + 1)
dropWhileClosingBracket (')':xs) 1 = xs
dropWhileClosingBracket (')':xs) count = dropWhileClosingBracket xs (count - 1)
dropWhileClosingBracket (x:xs) count = dropWhileClosingBracket xs count

invalidTermError :: String
invalidTermError = "invalid term"

parseTerm :: String -> Stack Term -> Term
parseTerm "" [term] = term
parseTerm "" stack = let rhs = peek stack
                         lhs = peek $ pop stack
                         updatedStack = pop stack
  in Application (parseTerm "" updatedStack) rhs
parseTerm term@('λ':argument:'.':body) stack
  | isAlpha argument = parseTerm "" (push stack (Abstraction argument (parseTerm body emptyStack)))
  | otherwise = error invalidTermError
parseTerm ('(':rest) stack = let lhs = takeWhileClosingBracket rest 1
                                 rhs = dropWhileClosingBracket rest 1
  in parseTerm rhs (push stack (parseTerm lhs emptyStack))
parseTerm (var:rest) stack
  | isAlpha var = parseTerm rest (push stack (Variable var))
  | otherwise = error invalidTermError
