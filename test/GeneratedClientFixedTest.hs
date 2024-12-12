{-# LANGUAGE TemplateHaskell #-}
module GeneratedClientFixedTest where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Client as C
import qualified User
import Data.List (nub)
import Test.QuickCheck.Gen (generate)
import ClientInternal (Result(..))

-- This file is a test for the generated client.
-- It uses QuickCheck to generate random data and test the client.
-- It uses model based testing.

-- How to run tests:
-- 1. Enter GHCi:
--    stack ghci
-- 2. In GHCi:
--    :set -isrc
--    :l src/HaskellPrisma.hs
--    generateModule example/schema.prisma testClient
--    :set -itestClient
--    :l test/GeneratedClientFixedTest.hs
--    main


-- Useful Generators

-- Generator for safe strings (no SQL injection characters)
genSafeString :: Gen String
genSafeString = listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- Generator for unique ints for ids
genUniqueInt :: Gen Int
genUniqueInt = choose (1, maxBound `div` 2)  -- Using half of maxBound to avoid overflow issues

genSafeStringUnique :: Gen String
genSafeStringUnique = do
  s <- genSafeString
  i <- genUniqueInt
  return $ s ++ show i

-- Generator for User Values
instance Arbitrary User.Value where
  arbitrary = oneof [
      User.Id <$> genUniqueInt
    , User.Email <$> genSafeStringUnique
    , User.Name <$> genSafeString
    , User.Dob <$> arbitrary
    ]

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
    email <- User.Email <$> genSafeStringUnique
    name <- User.Name <$> genSafeString
    dob <- User.Dob <$> arbitrary
    hasId <- arbitrary
    id <- if hasId 
          then pure <$> (User.Id <$> genUniqueInt)
          else return []
    return $ CreateArg $ [email, name, dob] ++ id

newtype UpdateArg = UpdateArg { unUpdateArg :: [User.Update] }
  deriving (Show)

instance Arbitrary UpdateArg where
  arbitrary = do
    email <- User.UEmail <$> C.StringSet <$> genSafeStringUnique
    name <- User.UName <$> C.StringSet <$> genSafeString
    dob <- User.UDob <$> C.IntSet <$> arbitrary
    updateEmail::Bool <- arbitrary
    updateName::Bool <- arbitrary
    updateDob::Bool <- arbitrary
    return $ UpdateArg $ concat [
        if updateEmail then [email] else [],
        if updateName then [name] else [], 
        if updateDob then [dob] else []
      ]

-- Property: Create operation should return OK and findAll should find the created user
prop_createUser_then_findAll :: CreateArg -> Property
prop_createUser_then_findAll (CreateArg values) = monadicIO $ do
  createResult <- run $ User.create values
  case createResult of
    SqlError err -> return $ counterexample ("Create failed: " ++ err) False
    OK () -> do
      -- Find the created user and verify values match
      findResult <- run $ User.findMany []  -- Get all users to find the created one
      case findResult of
        SqlError err -> return $ counterexample ("Find failed: " ++ err) False
        OK users -> return $ property $ not $ null $ filter (matchesValues values) users
  where
    matchesValues vals user = all (matchesValue user) vals
    matchesValue user val = case val of
      User.Email e -> User.getUserEmail user == e  
      User.Name n -> User.getUserName user == n
      User.Dob d -> User.getUserDob user == d
      User.Id i -> User.getUserId user == i

convertValueToQuery :: User.Value -> User.Query
convertValueToQuery (User.Email e) = User.QEmail (C.StringEquals e)
convertValueToQuery (User.Name n) = User.QName (C.StringEquals n)
convertValueToQuery (User.Dob d) = User.QDob (C.IntEquals d)
convertValueToQuery (User.Id i) = User.QId (C.IntEquals i)


-- Property: Find after create should return OK and find the user
prop_createUser_then_query_with_findMany :: CreateArg -> Property
prop_createUser_then_query_with_findMany (CreateArg values) = monadicIO $ do
  createResult <- run $ User.create values
  case createResult of
    SqlError err -> return $ counterexample ("Create failed: " ++ err) False
    OK () -> do
      findResult <- run $ User.findMany (map convertValueToQuery values)
      case findResult of
        SqlError err -> return $ counterexample ("Find failed: " ++ err) False
        OK users -> return $ property $ not (null users)

convertUpdateToValues :: User.Update -> User.Value
convertUpdateToValues (User.UEmail (C.StringSet e)) = User.Email e
convertUpdateToValues (User.UName (C.StringSet n)) = User.Name n
convertUpdateToValues (User.UDob (C.IntSet d)) = User.Dob d
convertUpdateToValues (User.UId (C.IntSet i)) = User.Id i

convertUpdateToQuery :: User.Update -> User.Query
convertUpdateToQuery (User.UEmail (C.StringSet e)) = User.QEmail (C.StringEquals e)
convertUpdateToQuery (User.UName (C.StringSet n)) = User.QName (C.StringEquals n)
convertUpdateToQuery (User.UDob (C.IntSet d)) = User.QDob (C.IntEquals d)
convertUpdateToQuery (User.UId (C.IntSet i)) = User.QId (C.IntEquals i)

-- Property: Create and update a user, then find should return OK with updated user
prop_updateUnique :: CreateArg -> UpdateArg -> Property
prop_updateUnique (CreateArg values) (UpdateArg updates) = 
  not (null updates) ==> monadicIO $ do
    createResult <- run $ User.create values
    case createResult of
      SqlError err -> return $ counterexample ("Create failed: " ++ err) False
      OK () -> do
        updateResult <- run $ User.updateUnique (map convertValueToQuery values) updates
        case updateResult of
          SqlError err -> return $ counterexample ("Update failed: " ++ err) False
          OK () -> do
            findResult <- run $ User.findUnique (map convertUpdateToQuery updates)
            case findResult of
              SqlError err -> return $ counterexample ("Find failed: " ++ err) False
              OK user -> return $ property $ all (updateApplied user) updates
  where
    updateApplied user update = case update of
      User.UEmail (C.StringSet e) -> 
        if User.getUserEmail user == e 
        then True 
        else error $ "Email mismatch: expected " ++ e ++ " but got " ++ User.getUserEmail user
      User.UName (C.StringSet n) -> 
        if User.getUserName user == n 
        then True 
        else error $ "Name mismatch: expected " ++ n ++ " but got " ++ User.getUserName user
      User.UDob (C.IntSet d) -> 
        if User.getUserDob user == d 
        then True 
        else error $ "DOB mismatch: expected " ++ show d ++ " but got " ++ show (User.getUserDob user)
      User.UId (C.IntSet i) -> 
        if User.getUserId user == i 
        then True 
        else error $ "ID mismatch: expected " ++ show i ++ " but got " ++ show (User.getUserId user)
      _ -> True  -- For other update types

-- Property: Delete should return OK and user should not be found after
prop_deleteUser :: CreateArg -> Property
prop_deleteUser (CreateArg values) = monadicIO $ do
    -- First create a user
    createResult <- run $ User.create values
    case createResult of
      SqlError err -> return $ counterexample ("Create failed: " ++ err) False
      OK () -> do
        -- Delete the user using queries based on their values
        deleteResult <- run $ User.deleteUnique (map convertValueToQuery values)
        case deleteResult of
          SqlError err -> return $ counterexample ("Delete failed: " ++ err) False
          OK () -> do
            -- Try to find the deleted user
            findResult <- run $ User.findUnique (map convertValueToQuery values)
            case findResult of
              SqlError _ -> return $ property True  -- Expected error when not found
              OK _ -> return $ counterexample "User still exists after deletion" False

-- Run all tests
return []
runTests = $quickCheckAll

main :: IO Bool
main = runTests

