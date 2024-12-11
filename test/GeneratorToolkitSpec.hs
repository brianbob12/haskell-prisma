module GeneratorToolkitSpec where

import Test.HUnit
import GeneratorToolkit

tests :: Test
tests = TestList [
    TestLabel "generateRecordData - empty" testRecordEmpty,
    TestLabel "generateRecordData - single arg" testRecordSingle,
    TestLabel "generateRecordData - multiple args" testRecordMultiple,
    TestLabel "generateSumData - empty" testSumEmpty,
    TestLabel "generateSumData - single arg" testSumSingle,
    TestLabel "generateSumData - multiple args" testSumMultiple
  ]

testRecordEmpty :: Test
testRecordEmpty = TestCase $ assertEqual
    "Should generate record with no fields"
    "data Person = Person { \n  }\n"
    (generateRecordData "Person" [])

testRecordSingle :: Test
testRecordSingle = TestCase $ assertEqual
    "Should generate record with single field"
    "data Person = Person { \n  name :: String\n  }\n"
    (generateRecordData "Person" [DataArg "name" "String"])

testRecordMultiple :: Test
testRecordMultiple = TestCase $ assertEqual
    "Should generate record with multiple fields"
    "data Person = Person { \n  name :: String\n  age :: Int\n  }\n"
    (generateRecordData "Person" [
      DataArg "name" "String",
      DataArg "age" "Int"
    ])

testSumEmpty :: Test
testSumEmpty = TestCase $ assertEqual
    "Should handle empty sum type"
    ""
    (generateSumData "Shape" [])

testSumSingle :: Test
testSumSingle = TestCase $ assertEqual
    "Should generate sum type with single constructor"
    "data Shape = \n  Circle Double\n"
    (generateSumData "Shape" [DataArg "Circle" "Double"])

testSumMultiple :: Test
testSumMultiple = TestCase $ assertEqual
    "Should generate sum type with multiple constructors"
    "data Shape = \n  Circle Double\n  Rectangle Double Double\n"
    (generateSumData "Shape" [
      DataArg "Circle" "Double",
      DataArg "Rectangle" "Double Double"
    ])

main :: IO Counts
main = runTestTT tests 

{-
How to run tests:
1. Enter GHCi:
   stack ghci

2. In GHCi:
   :set -isrc
   :l test/GeneratorToolkitSpec.hs
   main
-}

