module Main where

import Examples.Terms (allExamples, simpleApplication, currySubstitution2)
import Parser (parseTerm, Token (Variable))
import Substitution (substitute)

parseTest :: IO ()
parseTest = mapM_ (print . parseTerm) allExamples

main :: IO ()
main = print $ substitute (parseTerm currySubstitution2) 'x' (Variable 'y')