module Main where

import Examples.Terms (variable, simpleAbstraction, simpleApplication, abstraction, complexApplication, composed, parens, allExamples)
import Parser (parseTerm)

main :: IO ()
main = mapM_ (print . parseTerm) allExamples