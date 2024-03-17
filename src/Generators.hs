module Generators where

import Terms (Term (..), ApplicativeTerm (..), Combinator (..), NamelessTerm (..))

generateTerm :: Term -> String
generateTerm (Variable var) = [var]
generateTerm (Application lhs rhs) =
  let generatedLhs = case lhs of
        var@(Variable _) -> generateTerm var
        application@(Application _ _) -> generateTerm application
        abstraction@(Abstraction _ _) -> '(' : generateTerm abstraction ++ [')']
      generatedRhs = case rhs of
        var@(Variable _) -> generateTerm var
        application@(Application _ _) -> '(' : generateTerm application ++ [')']
        abstraction@(Abstraction _ _) -> '(' : generateTerm abstraction ++ [')']
   in generatedLhs ++ generatedRhs
generateTerm (Abstraction argument body) = 'λ' : argument : '.' : generateTerm body

generateNamelessTerm :: NamelessTerm -> String
generateNamelessTerm (NamelessVariable var) = show var
generateNamelessTerm (NamelessApplication lhs rhs) =
  let generatedLhs = case lhs of
        var@(NamelessVariable _) -> generateNamelessTerm var
        application@(NamelessApplication _ _) -> generateNamelessTerm application
        abstraction@(NamelessAbstraction _) -> '(' : generateNamelessTerm abstraction ++ [')']
      generatedRhs = case rhs of
        var@(NamelessVariable _) -> generateNamelessTerm var
        application@(NamelessApplication _ _) -> '(' : generateNamelessTerm application ++ [')']
        abstraction@(NamelessAbstraction _) -> '(' : generateNamelessTerm abstraction ++ [')']
   in generatedLhs ++ generatedRhs
generateNamelessTerm (NamelessAbstraction body) = 'λ' : generateNamelessTerm body

generateCombinator :: Combinator -> String
generateCombinator KCombinator = "K"
generateCombinator SCombinator = "S"

generateApplicativeTerm :: ApplicativeTerm -> String
generateApplicativeTerm (ApplicativeVariable var) = [var]
generateApplicativeTerm (ApplicativeApplication lhs rhs) =
  let generatedLhs = generateApplicativeTerm lhs
      generatedRhs = case rhs of
        var@(ApplicativeVariable _) -> generateApplicativeTerm var
        application@(ApplicativeApplication _ _) -> '(' : generateApplicativeTerm application ++ [')']
        combinator@(ApplicativeCombinator _) -> generateApplicativeTerm combinator
   in generatedLhs ++ generatedRhs
generateApplicativeTerm (ApplicativeCombinator combinator) = generateCombinator combinator