module Main where

import Test.HUnit
import qualified System.Exit as Exit
import Suites.Parser (parserTests)

tests :: Test
tests = TestList [parserTests] 

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess