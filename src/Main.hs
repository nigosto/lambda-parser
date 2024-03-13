module Main where

import Data.Map (empty, lookup)
import Generator (generateNamelessTerm, generateTerm)
import Parser (Term (Variable), parseTerm)
import Substitution.Named (substitute)
import Substitution.Nameless (substituteNameless)
import System.IO (BufferMode (BlockBuffering), hSetBuffering, stdout)
import Transformer (toNamed, toNameless)
import Prelude hiding (lookup)

main :: IO ()
main = do
  hSetBuffering stdout $ BlockBuffering $ Just 1
  term <- putStr "Input term: " >> getLine
  var <- putStr "Input variable to substitute: " >> getLine -- not using getChr intentionally
  if length var /= 1
    then error "please provide valid variable"
    else do
      let (namelessTerm, context) = toNameless $ parseTerm term
          substituteVariable = head var `lookup` context
      substituteTerm <- putStr "Input substitution term: " >> getLine
      case substituteVariable of
        Nothing -> putStrLn term
        Just index ->
          let (substituteNamelessTerm, updatedContext) = toNameless $ parseTerm substituteTerm
           in putStrLn $ generateTerm $ toNamed (substituteNameless namelessTerm index substituteNamelessTerm) updatedContext

-- let substituteVariable = head var
-- substituteTerm <- putStr "Input substitution term: " >> getLine
-- putStrLn $ generateTerm $ substitute (parseTerm term) substituteVariable (parseTerm substituteTerm)