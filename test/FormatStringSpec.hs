module FormatStringSpec where

import Test.HUnit
import FormatString (formatString)

tests :: Test
tests = TestList [
    TestLabel "formatString - no variables" testNoVariables,
    TestLabel "formatString - single variable" testSingleVariable,
    TestLabel "formatString - multiple variables" testMultipleVariables,
    TestLabel "formatString - repeated variables" testRepeatedVariables,
    TestLabel "formatString - missing variable" testMissingVariable
  ]

testNoVariables :: Test
testNoVariables = TestCase $ assertEqual
    "Should return same string when no variables present"
    "Hello, World!"
    (formatString "Hello, World!" [])

testSingleVariable :: Test
testSingleVariable = TestCase $ assertEqual
    "Should replace single variable"
    "Hello, John!"
    (formatString "Hello, ${name}!" [("name", "John")])

testMultipleVariables :: Test
testMultipleVariables = TestCase $ assertEqual
    "Should replace multiple different variables"
    "Hello, John! You are 30 years old."
    (formatString "Hello, ${name}! You are ${age} years old." [
      ("name", "John"),
      ("age", "30")
    ])

testRepeatedVariables :: Test
testRepeatedVariables = TestCase $ assertEqual
    "Should replace repeated variables"
    "Hello John! How are you John?"
    (formatString "Hello ${name}! How are you ${name}?" [
      ("name", "John")
    ])

testMissingVariable :: Test
testMissingVariable = TestCase $ assertEqual
    "Should leave unreplaced variables as is"
    "Hello, ${unknown}!"
    (formatString "Hello, ${unknown}!" [("name", "John")])

main :: IO Counts
main = runTestTT tests

{-
How to run tests:
1. Enter GHCi:
   stack ghci

2. In GHCi:
   :set -isrc
   :l test/FormatStringSpec.hs
   main
-}