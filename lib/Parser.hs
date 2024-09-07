{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

module Parser where
import Data.Bifunctor
import Control.Applicative
import Data.Char (isAlpha, isDigit)
import Data.List.NonEmpty (NonEmpty, fromList, cons)
import Debug.Trace (trace)

data Term =
  Variable String |
  Application Term Term |
  Abstraction String Term
  deriving (Show, Eq)

data ParsingError = EndOfLine | UnexpectedCharacter Char 
  deriving (Eq, Show)

newtype Parser a = Parser {run :: String -> Either ParsingError (a, String)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ fmap (first f) . p

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \input -> Right (x, input)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser f) <*> pa = Parser $ \input -> do
    (output, rest) <- f input
    run (output <$> pa) rest

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser a) >>= f = Parser $ \input -> do
    (output, rest) <- a input
    run (f output) rest

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const $ Left EndOfLine

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    case p1 input of
      -- Returning only the first error in case the second parser also fails.
      Left error -> p2 input
      Right (output, rest) -> Right (output, rest)

(<@>) :: Parser a -> Parser [a] -> Parser [a]
p1 <@> p2 = (:) <$> p1 <*> p2

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \case
    [] -> Left EndOfLine
    x:xs
      | predicate x -> Right (x, xs)
      | otherwise -> Left $ UnexpectedCharacter x

char :: Char -> Parser Char
char c = satisfy (== c)

alphaChar :: Parser Char
alphaChar = satisfy (\x -> isAlpha x && x /= 'λ')

string :: String -> Parser String
string = traverse char

digit :: Parser Char
digit = satisfy isDigit

integer :: Parser String
integer = many digit

variable :: Parser String
variable = alphaChar <@> integer

parseVariable :: Parser Term
parseVariable = Variable <$> variable

parseAbstraction :: Parser Term
parseAbstraction = uncurry Abstraction <$> ((char 'λ' *> ((,) <$> variable) <* char '.') <*> parseTerm)

-- Abstraction is not atomic, because it needs to be wrapped in braces if it needs to be
-- the left argument of the application
atomicExpression :: Parser Term
atomicExpression = parseVariable <|> (char '(' *> parseTerm <* char ')')

end :: Parser [Term]
end = Parser $ \input -> case input of
  [] -> Right ([], [])
  (x:xs) -> if isAlpha x && x /= 'λ' then Left $ UnexpectedCharacter $ head input else Right ([], input)

-- Break the left-recursive grammar using additional rules.
applicative :: Parser [Term]
applicative = nonEmptyApplicative <|> end

nonEmptyApplicative :: Parser [Term]
nonEmptyApplicative = (atomicExpression <|> parseAbstraction) <@> applicative

-- In order to keep the application left-associative, first parse every
-- subterm of the application chain separately and after than combine them
-- into one application chain.
concatApplications :: [Term] -> Term
concatApplications g = foldl Application (Application (head g) (head $ tail g)) (tail $ tail g)

parseApplication :: Parser Term
parseApplication =  concatApplications <$> (atomicExpression <@> nonEmptyApplicative)

parseTerm :: Parser Term
parseTerm = parseAbstraction <|> parseApplication <|> parseVariable

parse :: String -> Either ParsingError Term
parse input = fst <$> run parseTerm input
