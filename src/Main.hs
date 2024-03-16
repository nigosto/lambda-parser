module Main where

import Data.Map (empty, lookup)
import Generator (generateNamelessTerm, generateTerm)
import Parser (Term (Variable), parseTerm)
import Substitution.Named (substitute)
import Substitution.Nameless (substituteNameless)
import System.IO (BufferMode (BlockBuffering), hSetBuffering, stdout)
import Transformer (toNamed, toNameless)
import Prelude hiding (lookup)
import Stack (emptyStack)
import Interaction (getSubstitutionType, getOutputTermType, doSubstituteNameless, TermType (Named), doSubstituteNamed)

main :: IO ()
main = do
  hSetBuffering stdout $ BlockBuffering $ Just 1
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
