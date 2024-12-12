module TestPrismaParser where

import PrismaParser
import PrismaParserInternal
import Parser (parse)
import PrismaSchemaRaw

-- import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck

prop_schema :: Schema -> Bool
prop_schema s = parseSchema (toString s) == Right s

prop_enum :: EnumType -> Bool
prop_enum e = parse enumP (toString e) == Right e

prop_model :: Model -> Bool
prop_model m = parse modelP (toString m) == Right m

prop_field :: Field -> Bool
prop_field f = parse fieldP (toString f) == Right f

prop_fieldType :: FieldType -> Bool
prop_fieldType t = parse fieldTypeP (toString t) == Right t

prop_attribute :: Attribute -> Bool
prop_attribute a = parse attributeP (toString a) == Right a

prop_expression :: Expression -> Bool
prop_expression e = parse expressionP (toString e) == Right e

runQC :: IO ()
runQC = do
  putStrLn "Round-trip test of expression parsing"
  quickCheck prop_expression
  putStrLn "Round-trip test of attribute parsing"
  quickCheck prop_attribute
  putStrLn "Round-trip test of field type parsing"
  quickCheck prop_fieldType
  putStrLn "Round-trip test of field parsing"
  quickCheck prop_field
  putStrLn "Round-trip test of enum parsing"
  quickCheck prop_enum
  putStrLn "Round-trip test of model parsing"
  quickCheck prop_model
  putStrLn "Round-trip test of schema parsing"
  quickCheck prop_schema

instance Arbitrary Schema where
  arbitrary = do
    url <- (arbitrary :: Gen DatabaseURL)
    es <- (arbitrary :: Gen EnumType) `vectorUpTo` 10
    ms <- (arbitrary :: Gen Model) `vectorUpTo` 10
    return $ Schema url es ms where

instance Arbitrary DatabaseURL where
  arbitrary = do
    url <- listOf $ elements ['0' .. 'z']
    elements [DirectURL url, EnvironmentVariable url]

instance Arbitrary EnumType where
  arbitrary = do
    n <- arbitraryName
    vals <- listOf arbitraryName
    return $ EnumDefinition n vals

instance Arbitrary Model where
  arbitrary = do
    n <- arbitraryName
    fs <- (arbitrary :: Gen Field) `vectorUpTo` 30
    return $ Model n fs

instance Arbitrary Field where
  arbitrary = do
    n <- arbitraryName
    t <- (arbitrary :: Gen FieldType)
    as <- (arbitrary :: Gen Attribute) `vectorUpTo` 7
    return $ Field n t as

instance Arbitrary FieldType where
  arbitrary =
    let primitives = elements [IntField, StringField, BooleanField, FloatField, DecimalField, DateTimeField, JsonField, BytesField] in
    let m = arbitraryName >>= \n -> return $ ModelField n in
    let listT = (arbitrary :: Gen FieldType) >>= \t -> return $ ListOf t in
    let optT = (arbitrary :: Gen FieldType) >>= \t -> return $ OptionalField t in
    frequency [(8, primitives), (1, m), (1, listT), (1, optT)]

instance Arbitrary Attribute where
  arbitrary = 
    let basic = elements [IDAttribute, UniqueAttribute, UpdatedAtAttribute, IgnoreAttribute] in
    let dfault = (arbitrary :: Gen Expression) >>= \e -> return $ DefaultAttribute e in
    let mapAttr = arbitraryName >>= \s -> return $ MapAttribute s in
    let rel = listOf arbitraryName >>= \fs -> listOf arbitraryName >>= \rs -> return $ RelationAttribute fs rs in
    frequency [(4, basic), (1, dfault), (1, mapAttr), (1, rel)]

instance Arbitrary Expression where
  arbitrary = 
    let basic = elements [IntExpression AutoIncrementExpression, StringExpression CuidExpression, StringExpression UuidExpression, DateTimeExpression NowExpression] in
    let intL = (arbitrary :: Gen Int) >>= \x -> return $ IntExpression (IntLiteralExpression x) in
    let stringL = (listOf $ elements (['a' .. 'z'])) >>= \s -> return $ StringExpression (StringLiteralExpression s) in
    frequency [(4, basic), (1, intL), (1, stringL)]

arbitraryName :: Gen String
arbitraryName = do
  start <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  return (start : rest)

vectorUpTo :: Gen a -> Int -> Gen [a]
vectorUpTo arb n = do
  len <- chooseInt (0, n)
  vectorOf n arb
