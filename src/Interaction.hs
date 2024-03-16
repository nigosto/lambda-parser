module Interaction where

import Prelude hiding (lookup)
import Generator (generateTerm, generateNamelessTerm)
import Parser (parseTerm)
import Libs.Stack (emptyStack)
import Substitution.Named (substitute)
import Transformer (toNameless, toNamed)
import Data.Map (empty, lookup)
import Substitution.Nameless (substituteNameless)

data TermType = Nameless | Named deriving (Show, Eq)

parseTermType :: String -> TermType
parseTermType "named" = Named
parseTermType "Named" = Named
parseTermType "nameless" = Nameless
parseTermType "Nameless" = Nameless
parseTermType _ = error "Invalid term type"

getOutputTermType :: IO TermType
getOutputTermType = putStr "Please enter the type of the output terms (named, nameless): " >> parseTermType <$> getLine

getSubstitutionType :: IO TermType
getSubstitutionType = putStr "Please enter the type of the substitution to be used (named, nameless): " >> parseTermType <$> getLine

doSubstituteNamed :: String -> String -> String -> String
doSubstituteNamed term var substituteTerm = generateTerm $ substitute (parseTerm term emptyStack) (head var) (parseTerm substituteTerm emptyStack)

doSubstituteNameless :: String -> String -> String -> TermType -> String
doSubstituteNameless term var substituteTerm outputTermType =
  let (namelessTerm, context) = toNameless (parseTerm term emptyStack) empty
      substituteVariable = head var `lookup` context
  in case substituteVariable of
    Nothing -> term
    Just index ->
      let (substituteNamelessTerm, updatedContext) = toNameless (parseTerm substituteTerm emptyStack) context
          result = substituteNameless namelessTerm index substituteNamelessTerm
       in if outputTermType == Named 
          then generateTerm $ toNamed result updatedContext
          else generateNamelessTerm result
