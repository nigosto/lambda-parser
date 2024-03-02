module Generator where
import Parser (Token(Variable, Application, Abstraction))

generateTerm :: Token -> String
generateTerm (Variable var) = [var]

generateTerm (Application lhs rhs) = 
    let generatedLhs = case lhs of
          var@(Variable _) -> generateTerm var
          _ -> '(':generateTerm lhs ++ [')']
        generatedRhs = case rhs of
          var@(Variable _) -> generateTerm var
          _ -> '(':generateTerm rhs ++ [')']
    in generatedLhs ++ generatedRhs

generateTerm (Abstraction argument body) = 'λ':argument:'.':generateTerm body