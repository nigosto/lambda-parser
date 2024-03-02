module Parser (parseTerm, Token(Variable, Application, Abstraction)) where
import Data.Char (isAlpha)

data Token =
  Variable Char |
  Application Token Token |
  Abstraction Char Token
  deriving (Show)

takeWhileClosingParen :: String -> Int-> String
takeWhileClosingParen ('(':xs) count = '(':takeWhileClosingParen xs (count + 1)
takeWhileClosingParen (')':xs) 1 = ""
takeWhileClosingParen (')':xs) count = ')':takeWhileClosingParen xs (count - 1)
takeWhileClosingParen (x:xs) count = x:takeWhileClosingParen xs count

dropWhileClosingParen :: String -> Int-> String
dropWhileClosingParen ('(':xs) count = dropWhileClosingParen xs (count + 1)
dropWhileClosingParen (')':xs) 1 = xs
dropWhileClosingParen (')':xs) count = dropWhileClosingParen xs (count - 1)
dropWhileClosingParen (x:xs) count = dropWhileClosingParen xs count

invalidTermError :: String
invalidTermError = "invalid term"

parseTerm :: String -> Token
parseTerm [var]
  | isAlpha var = Variable var
  | otherwise = error invalidTermError

parseTerm ('λ':argument:'.':body)
  | isAlpha argument = Abstraction argument $ parseTerm body
  | otherwise = error invalidTermError

parseTerm ('(':term) = let lhs = takeWhileClosingParen term 1
                           rhs = dropWhileClosingParen term 1
  in if length (init term) == length lhs 
     then parseTerm (init term) 
     else Application (parseTerm lhs) (parseTerm rhs)

parseTerm (var:term)
  | isAlpha var = Application (Variable var) $ parseTerm term
  | otherwise = error invalidTermError

parseTerm _ = error invalidTermError