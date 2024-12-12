module PrismaParser (parseSchema) where

import PrismaSchemaRaw (Schema)

import Parser (parse)

-- internal functions are in a separate module so that they are
-- 1) testable 
-- 2) not exposed to the user of the library
import PrismaParserInternal (schemaP)

parseSchema :: String -> Either String Schema
parseSchema = parse schemaP
