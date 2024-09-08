{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Char (isAlpha, isDigit)
import Data.List.NonEmpty (NonEmpty ((:|)), cons, fromList, toList)
import Debug.Trace (trace)

data Term
  = Variable String
  | Application Term Term
  | Abstraction String Term
  deriving (Show, Eq)

data ParsingError
  = EndOfLine
  | UnexpectedCharacter Char
  | InvalidOperands
  | InputNotExhausted
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

instance MonadPlus Parser

isIdentifier :: Char -> Bool
isIdentifier c = isAlpha c && 'λ' /= c

(<:>) :: Parser a -> Parser [a] -> Parser [a]
p1 <:> p2 = liftA2 (:) p1 p2

peek :: Parser a -> Parser a
peek (Parser p) = Parser $ \input -> do
  (output, _) <- p input
  Right (output, input)

withEmpty :: Parser a -> Parser [b]
withEmpty p = [] <$ p

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \case
  [] -> Left EndOfLine
  x : xs
    | predicate x -> Right (x, xs)
    | otherwise -> Left $ UnexpectedCharacter x

char :: Char -> Parser Char
char c = satisfy (== c)

identifier :: Parser Char
identifier = satisfy isIdentifier

digit :: Parser Char
digit = satisfy isDigit

integer :: Parser String
integer = many digit

eof :: Parser ()
eof = Parser $ \case
  [] -> Right ((), [])
  _ -> Left InputNotExhausted

variable :: Parser String
variable = identifier <:> integer

-- Break the left-recursive grammar using additional rules.
applicationOperator :: Parser Term
applicationOperator = parseVariableTerm <|> (char '(' *> parseTerm <* char ')')

applicationEnd :: Parser [Term]
applicationEnd = withEmpty eof <|> (withEmpty . peek . satisfy $ (not . isIdentifier))

applicationOperand :: Parser [Term]
applicationOperand = toList <$> nonEmptyApplicationOperand <|> applicationEnd

nonEmptyApplicationOperand :: Parser (NonEmpty Term)
nonEmptyApplicationOperand = fromList <$> (applicationOperator <|> parseAbstractionTerm) <:> applicationOperand

-- In order to keep the application left-associative, first parse every
-- subterm of the application chain separately and after than combine them
-- into a single nested application.
concatApplications :: NonEmpty Term -> Parser Term
concatApplications (x :| []) = Parser $ const $ Left InvalidOperands
concatApplications terms = Parser $ Right . (foldl1 Application terms,)

parseVariableTerm :: Parser Term
parseVariableTerm = Variable <$> variable

parseApplicationTerm :: Parser Term
parseApplicationTerm = liftA2 cons applicationOperator nonEmptyApplicationOperand >>= concatApplications

parseAbstractionTerm :: Parser Term
parseAbstractionTerm = uncurry Abstraction <$> liftA2 (,) (char 'λ' *> variable <* char '.') parseTerm

parseTerm :: Parser Term
parseTerm = msum [parseAbstractionTerm, parseApplicationTerm, parseVariableTerm]

parse :: String -> Either ParsingError Term
parse input = fst <$> run parseTerm input
