{-# LANGUAGE TupleSections #-}

module Parser where

import Libs.Parser
import Terms
import Data.List.NonEmpty (NonEmpty ((:|)), cons, fromList, toList)
import Control.Applicative
import Control.Monad

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
