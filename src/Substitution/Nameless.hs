module Substitution.Nameless where
import Transformer (NamelessTerm (NamelessVariable, NamelessApplication, NamelessAbstraction), NamingContext)

shift :: Int -> Int -> NamelessTerm -> NamelessTerm
shift c d var@(NamelessVariable k)
    | 0 <= k && k < c = var
    | otherwise = NamelessVariable (k + d)
shift c d (NamelessApplication lhs rhs) = NamelessApplication (shift c d lhs) (shift c d rhs)
shift c d (NamelessAbstraction depth body) = NamelessAbstraction depth (shift (c + 1) d body)

singleShift :: NamelessTerm -> NamelessTerm
singleShift = shift 0 1

substituteNameless :: NamelessTerm -> Int -> NamelessTerm -> NamelessTerm
substituteNameless (NamelessVariable var) x term
    | x == var = term
    | otherwise = NamelessVariable var
substituteNameless (NamelessApplication lhs rhs) x term =
    NamelessApplication (substituteNameless lhs x term) (substituteNameless rhs x term)
substituteNameless (NamelessAbstraction depth body) x term =
    NamelessAbstraction depth (substituteNameless body (x + 1) (singleShift term))