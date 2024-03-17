module Main where

import System.IO (BufferMode (BlockBuffering), hSetBuffering, stdout)
import Interaction (
  requestMode, 
  executeSubstitution,
  executeTransformationToApplicative, 
  executeBetaRedexesCount)

main :: IO ()
main = do
  hSetBuffering stdout $ BlockBuffering $ Just 1
  modeIndex <- requestMode
  case modeIndex of
    1 -> executeSubstitution
    2 -> executeTransformationToApplicative
    3 -> executeBetaRedexesCount
    _ -> putStrLn "Unrecognized operation!" >> main
