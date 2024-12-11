{-# LANGUAGE TemplateHaskell #-}
module GeneratedClientFixedTest where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Client as C
import qualified User
import Data.List (nub)

-- This file is a test for the generated client.
-- It uses QuickCheck to generate random data and test the client.
-- It uses model based testing.

-- How to run tests:
-- 1. Enter GHCi:
--    stack ghci
-- 2. In GHCi:
--    :set -isrc
--    :l lib.hs
--    generateClient example/schema.prisma testClient
--    :set -itestClient
--    :l test/GeneratedClientFixedTest.hs
--    main

-- Generator for User Values
instance Arbitrary User.Value where
  arbitrary = oneof [
      User.Id <$> arbitrary
    , User.Email <$> genSafeString
    , User.Name <$> genSafeString
    , User.Dob <$> arbitrary
    ]

-- Generator for safe strings (no SQL injection characters)
genSafeString :: Gen String
genSafeString = listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- Generator for IntQuery
instance Arbitrary C.IntQuery where
  arbitrary = oneof [
      C.IntEquals <$> arbitrary
    , C.IntNot <$> arbitrary
    , C.IntIn <$> (nub <$> listOf1 arbitrary)
    , C.IntNotIn <$> (nub <$> listOf1 arbitrary)
    , C.IntLt <$> arbitrary
    , C.IntLte <$> arbitrary
    , C.IntGt <$> arbitrary
    , C.IntGte <$> arbitrary
    ]

-- Generator for StringQuery
instance Arbitrary C.StringQuery where
  arbitrary = oneof [
      C.StringEquals <$> genSafeString
    , C.StringNot <$> genSafeString
    , C.StringIn <$> (nub <$> listOf1 genSafeString)
    , C.StringNotIn <$> (nub <$> listOf1 genSafeString)
    , C.StringContains <$> genSafeString
    , C.StringStartsWith <$> genSafeString
    , C.StringEndsWith <$> genSafeString
    ]

-- Generator for User Query
instance Arbitrary User.Query where
  arbitrary = oneof [
      User.QId <$> arbitrary
    , User.QEmail <$> arbitrary
    , User.QName <$> arbitrary
    , User.QDob <$> arbitrary
    , User.Or <$> listOf1 arbitrary
    , User.Not <$> arbitrary
    ]

-- Generator for IntUpdate
instance Arbitrary C.IntUpdate where
  arbitrary = oneof [
      C.IntSet <$> arbitrary
    , C.IntIncrement <$> arbitrary
    , C.IntDecrement <$> arbitrary
    , C.IntMultiply <$> arbitrary
    , C.IntDivide <$> (arbitrary `suchThat` (/= 0))  -- Avoid division by zero
    ]

-- Generator for StringUpdate
instance Arbitrary C.StringUpdate where
  arbitrary = C.StringSet <$> genSafeString

-- Generator for User Update
instance Arbitrary User.Update where
  arbitrary = oneof [
      User.UId <$> arbitrary
    , User.UEmail <$> arbitrary
    , User.UName <$> arbitrary
    , User.UDob <$> arbitrary
    ]

-- Change from type alias to newtype
-- This wrapper is needed so we can override Arbitrary [a]
newtype CreateArg = CreateArg { unCreateArg :: [User.Value] }
  deriving (Show)  -- Add Show derivation if needed

instance Arbitrary CreateArg where
  arbitrary = do
    email <- User.Email <$> genSafeString
    name <- User.Name <$> genSafeString
    dob <- User.Dob <$> arbitrary
    hasId <- arbitrary
    id <- if hasId 
          then pure <$> (User.Id <$> arbitrary)
          else return []
    return $ CreateArg $ [email, name, dob] ++ id

-- Property: Create operation should not fail
prop_createUser :: CreateArg -> Property
prop_createUser (CreateArg values) = monadicIO $ do
  run $ User.create values
  return True

-- Property: Find after create should return something
prop_findAfterCreate :: [User.Value] -> [User.Query] -> Property
prop_findAfterCreate values queries = monadicIO $ do
  run $ User.create values
  result <- run $ User.findMany queries
  return $ not (null result)

-- Property: Update should not fail
prop_updateUser :: [User.Query] -> [User.Update] -> Property
prop_updateUser queries updates = monadicIO $ do
  run $ User.updateMany queries updates
  return True

-- Property: Delete should not fail
prop_deleteUser :: [User.Query] -> Property
prop_deleteUser queries = monadicIO $ do
  run $ User.deleteMany queries
  return True

-- Run all tests
return []
runTests = $quickCheckAll

main :: IO Bool
main = runTests

