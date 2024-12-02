module PrismaParserInternal (schemaP, 
                             datasourceP, 
                             modelP, 
                             enumP, 
                             fieldP, 
                             fieldTypeP, 
                             attributeP, 
                             expressionP) where

import PrismaSchemaRaw (Schema, 
                        DatabaseURL, 
                        Model, 
                        EnumType, 
                        Field, 
                        FieldType, 
                        Attribute,
                        Expression,
                        IntExpression,
                        StringExpression,
                        DateTimeExpression)

import Parser (Parser, parse) -- will need to import more

schemaP :: Parser Schema
schemaP = undefined

datasourceP :: Parser DatabaseURL
datasourceP = undefined

modelP :: Parser Model
modelP = undefined

enumP :: Parser EnumType
enumP = undefined

fieldP :: Parser Field
fieldP = undefined

fieldTypeP :: Parser FieldType
fieldTypeP = undefined

attributeP :: Parser Attribute
attributeP = undefined

expressionP :: Parser Expression -- don't need individual parsers for each expression type
expressionP = undefined