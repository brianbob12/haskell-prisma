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
    parse enumP "enum Role {
        USER
        ADMIN
      }" ~?= Right $ EnumType {enumName="Role", values=["USER", "ADMIN"]}
    parse modelP "model User {
        id    Int     @id @default(autoincrement())
        email String  @unique
        name  String?
        role  Role    @default(USER)
      }" ~?= Right $ Model {modelName="User", 
                            fields=[Field {fieldName="id", fieldType=IntField, attributes=[IDAttribute, DefaultAttribute AutoIncrementExpression]}, 
                                    Field {fieldName="email", fieldType=StringField, attributes=[UniqueAttribute]}, 
                                    Field {fieldName="name", fieldType=(OptionalField StringField), attributes=[]}, 
                                    Field {fieldName="role", fieldType=(ModelField "Role"), attributes=[DefaultAttribute (EnumExpression "USER")]}
                            ]},
    parse datasourceP "datasource {
        provider = \"postgresql\"
        url      = env(\"DATABASE_URL\")
      }" ~?= Right $ DatabaseURL (EnvironmentVariable "DATABASE_URL"),
    parse schemaP "datasource db {
        provider = \"postgresql\"
        url      = env(\"DATABASE_URL\")
      }

      generator client {
        provider = \"prisma-client-js\"
      }

      model User {
        id      Int      @id @default(autoincrement())
        email   String   @unique
        name    String?
        role    Role     @default(USER)
      }

      enum Role {
        USER
        ADMIN
      }" ~?= Right $ Schema {
        databaseUrl = DatabaseURL (EnvironmentVariable "DATABASE_URL"),
        enumTypes = [EnumType {enumName="Role", values=["USER", "ADMIN"]}],
        models = [Model {
            modelName = "User",
            fields = [
                Field {fieldName="id", fieldType=IntField, attributes=[IDAttribute, DefaultAttribute AutoIncrementExpression]}, 
                Field {fieldName="email", fieldType=StringField, attributes=[UniqueAttribute]}, 
                Field {fieldName="name", fieldType=(OptionalField StringField), attributes=[]}, 
                Field {fieldName="role", fieldType=(ModelField "Role"), attributes=[DefaultAttribute (EnumExpression "USER")]}
            ]
        }]
      }

]