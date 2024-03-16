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

-- TODO: let the user decide which substitution to be used and how the
--       result should be displayed - either as named or nameless term
main :: IO ()
main = do
  hSetBuffering stdout $ BlockBuffering $ Just 1
  term <- putStr "Input term: " >> getLine
  var <- putStr "Input variable to substitute: " >> getLine -- not using getChr intentionally
  if length var /= 1
    then error "please provide valid variable"
    else do
      let (namelessTerm, context) = toNameless (parseTerm term emptyStack) empty
          substituteVariable = head var `lookup` context
      substituteTerm <- putStr "Input substitution term: " >> getLine
      case substituteVariable of
        Nothing -> putStrLn term
        Just index ->
          let (substituteNamelessTerm, updatedContext) = toNameless (parseTerm substituteTerm emptyStack) context
           in putStrLn $ generateTerm $ toNamed (substituteNameless namelessTerm index substituteNamelessTerm) updatedContext

-- let substituteVariable = head var
-- substituteTerm <- putStr "Input substitution term: " >> getLine
-- putStrLn $ generateTerm $ substitute (parseTerm term) substituteVariable (parseTerm substituteTerm)