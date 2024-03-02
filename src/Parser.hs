module Parser (parseTerm, Token(Variable, Application, Abstraction)) where
import Data.Char (isAlpha)

data Token =
  Variable Char |
  Application Token Token |
  Abstraction Char Token
  deriving (Show)

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

parseTerm :: String -> Token
parseTerm [var]
  | isAlpha var = Variable var
  | otherwise = error invalidTermError

parseTerm ('λ':argument:'.':body)
  | isAlpha argument = Abstraction argument $ parseTerm body
  | otherwise = error invalidTermError

parseTerm ('(':term) = let lhs = takeWhileClosingBracket term 1
                           rhs = dropWhileClosingBracket term 1
  in if length (init term) == length lhs 
     then parseTerm (init term) 
     else Application (parseTerm lhs) (parseTerm rhs)

parseTerm (var:term)
  | isAlpha var = Application (Variable var) $ parseTerm term
  | otherwise = error invalidTermError

parseTerm _ = error invalidTermError