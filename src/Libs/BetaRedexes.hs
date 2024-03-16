module Libs.BetaRedexes where
import Parser (Term (Variable, Application, Abstraction))
import Generator (generateTerm)

extractBetaRedexes :: Term -> [Term]
extractBetaRedexes (Variable _) = []
extractBetaRedexes term@(Application lhs@(Abstraction _ body) rhs) =
  term : extractBetaRedexes body ++ extractBetaRedexes rhs
extractBetaRedexes (Application lhs rhs) =
  extractBetaRedexes lhs ++ extractBetaRedexes rhs
extractBetaRedexes (Abstraction _ body) = extractBetaRedexes body

displayBetaRedexes :: Term -> IO ()
displayBetaRedexes term =
  mapM_ (putStrLn . generateTerm) $ extractBetaRedexes term