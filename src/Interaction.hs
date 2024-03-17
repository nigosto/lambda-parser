module Interaction where

import Prelude hiding (lookup)
import Generators (generateTerm, generateNamelessTerm, generateApplicativeTerm)
import Parser (parseTerm)
import Libs.Stack (emptyStack)
import Substitution.Named (substitute)
import Transformers (toNameless, toNamed, toApplicative)
import Data.Map (empty, lookup)
import Substitution.Nameless (substituteNameless)
import Libs.BetaRedexes (extractBetaRedexes)

requestMode :: IO Int
requestMode = do
  putStrLn "Please choose what kind of operation you want to do (1, 2, 3):"
  putStrLn "1) Substitution"
  putStrLn "2) Transformation from λ-term to Applicative term"
  putStrLn "3) Count beta-redexes in λ-term" >> read <$> getLine

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

executeSubstitution :: IO ()
executeSubstitution = do
  term <- putStr "Input term: " >> getLine
  var <- putStr "Input variable to substitute: " >> getLine -- not using getChr intentionally
  if length var /= 1
    then error "please provide valid variable"
    else do
      substituteTerm <- putStr "Input substitution term: " >> getLine
      substitutionType <- getSubstitutionType
      if substitutionType == Named
      then putStrLn $ doSubstituteNamed term var substituteTerm
      else do
        outputFormat <- getOutputTermType
        putStrLn $ doSubstituteNameless term var substituteTerm outputFormat

executeTransformationToApplicative :: IO ()
executeTransformationToApplicative =
  putStr "Input term: " >> flip parseTerm emptyStack <$> getLine >>= putStrLn . generateApplicativeTerm . toApplicative

executeBetaRedexesCount :: IO ()
executeBetaRedexesCount =
  putStr "Input term: " >> flip parseTerm emptyStack <$> getLine >>= mapM_ (putStrLn . generateTerm) . extractBetaRedexes
