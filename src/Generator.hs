module Generator where
import Parser (Term (Variable, Application, Abstraction))
import Transformer (NamelessTerm (NamelessVariable, NamelessApplication, NamelessAbstraction))

generateTerm :: Term -> String
generateTerm (Variable var) = [var]
generateTerm (Application lhs rhs) =
  let generatedLhs = case lhs of
        var@(Variable _) -> generateTerm var  
        application@(Application _ _) -> generateTerm application
        abstraction@(Abstraction _ _ ) -> '(':generateTerm abstraction ++ [')']
      generatedRhs = case rhs of
        var@(Variable _) -> generateTerm var
        application@(Application _ _) -> '(':generateTerm application ++ [')']
        abstraction@(Abstraction _ _) -> '(':generateTerm abstraction ++ [')']
  in generatedLhs ++ generatedRhs
generateTerm (Abstraction argument body) = 'λ':argument:'.':generateTerm body

generateNamelessTerm :: NamelessTerm -> String
generateNamelessTerm (NamelessVariable var) = show var
generateNamelessTerm (NamelessApplication lhs rhs) =
  let generatedLhs = case lhs of
        var@(NamelessVariable _) -> generateNamelessTerm var  
        application@(NamelessApplication _ _) -> generateNamelessTerm application
        abstraction@(NamelessAbstraction _ ) -> '(':generateNamelessTerm abstraction ++ [')']
      generatedRhs = case rhs of
        var@(NamelessVariable _) -> generateNamelessTerm var
        application@(NamelessApplication _ _) -> '(':generateNamelessTerm application ++ [')']
        abstraction@(NamelessAbstraction _) -> '(':generateNamelessTerm abstraction ++ [')']
  in generatedLhs ++ generatedRhs
generateNamelessTerm (NamelessAbstraction body) = 'λ':generateNamelessTerm body