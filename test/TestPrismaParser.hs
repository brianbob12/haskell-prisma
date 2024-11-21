module TestPrismaParser where

import Parser (parse)
import PrismaParser (parseSchema)
import PrismaParserInternal (..)
import PrismaSchemaRaw (..)

import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
--import Test.QuickCheck qualified as QC -- if we figure out a nice way to generate schema files and their corresponding Schema values

tests :: Test = TestList [
    parse expressionP "123" ~?= Right $ IntExpression (IntLiteralExpression 123),
    parse expressionP "-123" ~?= Right $ IntExpression (IntLiteralExpression (-123)),
    parse expressionP "autoincrement()" ~?= Right $ IntExpression AutoIncrementExpression,
    parse expressionP "cuid()" ~?= Right $ StringExpression CuidExpression,
    parse expressionP "uuid()" ~?= Right $ StringExpression UuidExpression,
    parse expressionP "now()" ~?= Right $ DateTimeExpression NowExpression,
    parse attributeP "@default(autoincrement())" ~?= Right $ DefaultAttribute (IntExpression AutoIncrementExpression),
    parse attributeP "@unique" ~?= Right $ UniqueAttribute,
    parse fieldTypeP "Int" ~?= Right $ IntField,
    parse fieldTypeP "String[]" ~?= Right $ ListOf StringField,
    parse fieldTypeP "Boolean?" ~?= Right $ OptionalField BooleanField,
    parse fieldTypeP "User" ~?= Right $ ModelField "User",
    parse fieldP "id String @id @default(cuid())" ~?= Right $ Field "id" StringField [IDAttribute, DefaultAttribute (StringExpression CuidExpression)]
]

-- TODO: add tests for parsing enums, models, and urls (the important things in curly braces)