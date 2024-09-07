module Suites.Parser where

import Parser
import Test.HUnit

parseVariableTest :: Test
parseVariableTest =
  TestLabel "parses variables" $
    TestCase (Right (Variable "x") @=? parse "x")

parseIndexedVariableTest :: Test
parseIndexedVariableTest =
  TestLabel "parses indexed variable" $
    TestCase (Right (Variable "x13") @=? parse "x13")

parseApplicationOfVariablesTest :: Test
parseApplicationOfVariablesTest =
  TestLabel "parses application of 2 variables" $
    TestCase (Right (Application (Variable "x") (Variable "y")) @=? parse "xy")

parseApplicationOfMultipleVariablesTest :: Test
parseApplicationOfMultipleVariablesTest =
  TestLabel "parses application of multiple variables with left associativity" $
    TestCase (Right (Application (Application (Variable "x") (Variable "y")) (Variable "z")) @=? parse "xyz")

parseApplicationsWithBracesTest :: Test
parseApplicationsWithBracesTest =
  TestLabel "parses application that includes braces with correct associativity" $
    TestList
      [ TestCase (Right (Application (Application (Variable "x") (Variable "y")) (Variable "z")) @=? parse "(xy)z"),
        TestCase (Right (Application (Variable "x") (Application (Variable "y") (Variable "z"))) @=? parse "x(yz)")
      ]

parseAbstractionWithVariableBodyTest :: Test
parseAbstractionWithVariableBodyTest =
  TestLabel "parses abstraction, which has variable as body" $
    TestCase (Right (Abstraction "x" (Variable "x")) @=? parse "λx.x")

parseAbstractionWithIndexedVariableBodyTest :: Test
parseAbstractionWithIndexedVariableBodyTest =
  TestLabel "parses abstraction, which has indexed variable as body" $
    TestCase (Right (Abstraction "x1" (Variable "x1")) @=? parse "λx1.x1")

parseNestedAbstractionsWithVariableBodyTest :: Test
parseNestedAbstractionsWithVariableBodyTest =
  TestLabel "parses nested abstractions, where the inner most has variable as body" $
    TestCase (Right (Abstraction "x1" (Abstraction "x2" (Variable "x2"))) @=? parse "λx1.λx2.x2")

parseAbstractionWithApplicationBodyTest :: Test
parseAbstractionWithApplicationBodyTest =
  TestLabel "parses abstraction, which has application as body" $
    TestCase (Right (Abstraction "x" (Application (Variable "x") (Variable "y"))) @=? parse "λx.xy")

parseNestedAbstractionsWithApplicationBodyTest :: Test
parseNestedAbstractionsWithApplicationBodyTest =
  TestLabel "parses nested abstractions, where the inner most has application as body" $
    TestCase (Right (Abstraction "x1" (Abstraction "x2" (Application (Variable "x1") (Variable "x2")))) @=? parse "λx1.λx2.x1x2")

parseComposedTermTest :: Test
parseComposedTermTest =
  TestLabel "parses term that contains as subterms every kind of terms" $
    TestList [
      TestCase (Right (Application (Abstraction "x" (Application (Application (Variable "a") (Variable "b")) (Variable "x"))) (Application (Variable "y") (Variable "z"))) @=? parse "(λx.abx)(yz)"),
      TestCase (Right (Abstraction "y" (Application (Variable "y") (Abstraction "a" (Application (Variable "a") (Variable "x"))))) @=? parse "λy.yλa.ax"),
      TestCase (Right (Application (Application (Abstraction "x" (Variable "x")) (Application (Abstraction "y" (Variable "y")) (Variable "z"))) (Variable "u")) @=? parse "(λx.x)((λy.y)z)u"),
      TestCase (Right (Abstraction "z" (Application (Application (Application (Abstraction "x" (Abstraction "y" (Variable "x"))) (Abstraction "u" (Variable "u"))) (Abstraction "u" (Application (Application (Variable "u") (Variable "z")) (Variable "u")))) (Variable "z")))  @=? parse "λz.(λx.λy.x)(λu.u)(λu.uzu)z"),
      TestCase (Right (Application (Abstraction "x" (Application (Variable "x") (Variable "z"))) (Application (Abstraction "y" (Application (Variable "z") (Variable "y"))) (Variable "z"))) @=? parse "(λx.xz)((λy.zy)z)")
    ]

parserTests :: Test
parserTests =
  TestLabel "Parser Combinator" $
    TestList
      [ parseVariableTest,
        parseIndexedVariableTest,
        parseApplicationOfVariablesTest,
        parseApplicationOfMultipleVariablesTest,
        parseApplicationsWithBracesTest,
        parseAbstractionWithVariableBodyTest,
        parseAbstractionWithIndexedVariableBodyTest,
        parseNestedAbstractionsWithVariableBodyTest,
        parseAbstractionWithApplicationBodyTest,
        parseNestedAbstractionsWithApplicationBodyTest,
        parseComposedTermTest
      ]