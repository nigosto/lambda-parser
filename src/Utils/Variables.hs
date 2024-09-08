module Utils.Variables where
import Terms (Term (..), ApplicativeTerm (ApplicativeVariable, ApplicativeApplication, ApplicativeCombinator))

freeVariables :: Term -> [String]
freeVariables (Variable x) = [x]
freeVariables (Application lhs rhs) = freeVariables lhs ++ freeVariables rhs
freeVariables (Abstraction argument body) = filter (/= argument) $ freeVariables body

applicativeFreeVariables :: ApplicativeTerm -> [String]
applicativeFreeVariables (ApplicativeVariable var) = [var]
applicativeFreeVariables (ApplicativeApplication lhs rhs) = 
  applicativeFreeVariables lhs ++ applicativeFreeVariables rhs
applicativeFreeVariables (ApplicativeCombinator _) = []