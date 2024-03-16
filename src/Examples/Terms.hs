module Examples.Terms where

variable :: String
variable = "x"

simpleApplication :: String
simpleApplication = "xy"

complexApplication :: String
complexApplication = "xyz"

simpleAbstraction :: String
simpleAbstraction = "λx.x"

abstraction :: String
abstraction = "λx.xy"

brackets :: String
brackets = "(xy)"

composed :: String
composed = "(λx.abx)(yz)"

currySubstitution1 :: String
currySubstitution1 = "λy.yλa.ax"

currySubstitution2 :: String
currySubstitution2 = "λy.λa.yax"

betaRedexes :: String
betaRedexes = "(λx.x)((λy.y)z)u"

singleBetaRedex :: String
singleBetaRedex = "λz.(λx.λy.x)(λu.u)(λu.uzu)z"

complexBetaRedexes :: String
complexBetaRedexes = "(λx.xz)((λy.zy)z)"

allExamples :: [String]
allExamples = [
  variable, 
  simpleApplication, 
  complexApplication, 
  simpleAbstraction, 
  abstraction, 
  brackets, 
  composed, 
  currySubstitution1, 
  currySubstitution2,
  betaRedexes,
  singleBetaRedex,
  complexBetaRedexes ]