module Generator where
import Parser (Term (Variable, Application, Abstraction))
import Transformer (NamelessTerm (NamelessVariable, NamelessApplication, NamelessAbstraction))

generateTerm :: Term -> String
generateTerm (Variable var) = [var]
generateTerm (Application lhs rhs) = concatMap (\term -> case term of
  var@(Variable _) -> generateTerm var
  _ -> '(':generateTerm term ++ [')']) [lhs, rhs]
generateTerm (Abstraction argument body) = 'λ':argument:'.':generateTerm body

generateNamelessTerm :: NamelessTerm -> String
generateNamelessTerm (NamelessVariable var) = show var
generateNamelessTerm (NamelessApplication lhs rhs) = concatMap (\term -> case term of
  var@(NamelessVariable _) -> generateNamelessTerm var
  _ -> '(':generateNamelessTerm term ++ [')']) [lhs, rhs]
generateNamelessTerm (NamelessAbstraction body) = 'λ':generateNamelessTerm body