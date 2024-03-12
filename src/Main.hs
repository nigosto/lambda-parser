module Main where

import Prelude hiding (lookup)
import System.IO (hSetBuffering, stdout, BufferMode (BlockBuffering))
import Parser (parseTerm, Term (Variable))
import Substitution.Named (substitute)
import Generator (generateTerm, generateNamelessTerm)
import Transformer (toNameless)
import Data.Map (empty, lookup)
import Substitution.Nameless (substituteNameless)

main :: IO ()
main = do
  hSetBuffering stdout $ BlockBuffering $ Just 1
  term <- putStr "Input term: " >> getLine
  var <- putStr "Input variable to substitute: " >> getLine -- not using getChr intentionally
  if length var /= 1 
  then error "please provide valid variable"
  else do
    let (namelessTerm, context) = toNameless (parseTerm term) 0 [] empty
        substituteVariable = head var `lookup` context 
    case substituteVariable of
      Nothing -> error "please provide valid variable"
      Just index -> do 
        substituteTerm <- putStr "Input substitution term: " >> getLine
        let (substituteNamelessTerm, _) = toNameless (parseTerm substituteTerm) 0 [] context
        putStrLn $ generateNamelessTerm $ substituteNameless namelessTerm index substituteNamelessTerm

    -- let substituteVariable = head var
    -- substituteTerm <- putStr "Input substitution term: " >> getLine
    -- putStrLn $ generateTerm $ substitute (parseTerm term) substituteVariable (parseTerm substituteTerm)