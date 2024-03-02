module Main where

import System.IO (hSetBuffering, stdout, BufferMode (BlockBuffering))
import System.Environment (getArgs)
import Parser (parseTerm)
import Substitution (substitute)
import Generator (generateTerm)

main :: IO ()
main = do
  hSetBuffering stdout $ BlockBuffering $ Just 1
  args <- getArgs
  term <- putStr "Input term: " >> getLine
  var <- putStr "Input variable to substitute: " >> getLine -- not using getChr intentionally
  if length var /= 1 
  then error "please provide valid variable"
  else do
    let substituteVariable = head var
    substituteTerm <- putStr "Input substitution term: " >> getLine
    let generatedTerm = generateTerm $ substitute (parseTerm term) substituteVariable $ parseTerm substituteTerm
    let termToPrint = if "pretty-lambda" `elem` args then map (\x -> if x == 'λ' then '\\' else x) generatedTerm else generatedTerm
    putStrLn termToPrint