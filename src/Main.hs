module Main where

import System.IO (hSetBuffering, stdout, BufferMode (BlockBuffering))
import Parser (parseTerm, Token (Variable))
import Substitution (substitute)
import Generator (generateTerm)

main :: IO ()
main = do
  hSetBuffering stdout $ BlockBuffering $ Just 1
  term <- putStr "Input term: " >> getLine
  var <- putStr "Input variable to substitute: " >> getLine -- not using getChr intentionally
  if length var /= 1 
  then error "please provide valid variable"
  else do
    let substituteVariable = head var
    substituteTerm <- putStr "Input substitution term: " >> getLine
    putStrLn $ generateTerm $ substitute (parseTerm term) substituteVariable (parseTerm substituteTerm)