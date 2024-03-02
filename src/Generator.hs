module Generator where
import Parser (Token(Variable, Application, Abstraction))

generateTerm :: Token -> String
generateTerm (Variable var) = [var]

generateTerm (Application lhs rhs) = concatMap (\term -> case term of
  var@(Variable _) -> generateTerm var
  _ -> '(':generateTerm term ++ [')']) [lhs, rhs]

generateTerm (Abstraction argument body) = 'λ':argument:'.':generateTerm body