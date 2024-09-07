module BetaRedexes where
  
import Terms (Term (..))
import Generators (generateTerm)

extractBetaRedexes :: Term -> [Term]
extractBetaRedexes (Variable _) = []
extractBetaRedexes term@(Application lhs@(Abstraction _ body) rhs) =
  term : extractBetaRedexes body ++ extractBetaRedexes rhs
extractBetaRedexes (Application lhs rhs) =
  extractBetaRedexes lhs ++ extractBetaRedexes rhs
extractBetaRedexes (Abstraction _ body) = extractBetaRedexes body