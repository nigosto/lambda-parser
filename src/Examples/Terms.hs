module Examples.Terms where

variable :: String
variable = "x"

simpleApplication :: String
simpleApplication = "xy"

complexApplication :: String
complexApplication = "(xy)z"

simpleAbstraction :: String
simpleAbstraction = "λx.x"

abstraction :: String
abstraction = "λx.xy"

parens :: String
parens = "(xy)"

composed :: String
composed = "(λx.(ab)x)(yz)"

currySubstitution1 :: String
currySubstitution1 = "λy.yλa.ax"

currySubstitution2 :: String
currySubstitution2 = "λy.λa.yax"

allExamples :: [String]
allExamples = [
  variable, 
  simpleApplication, 
  complexApplication, 
  simpleAbstraction, 
  abstraction, 
  parens, 
  composed, 
  currySubstitution1, 
  currySubstitution2 ]