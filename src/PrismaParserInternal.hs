module PrismaParserInternal {-(schemaP, 
                             datasourceP, 
                             modelP, 
                             enumP, 
                             fieldP, 
                             fieldTypeP, 
                             attributeP, 
                             expressionP) -} where

import Control.Applicative
import Data.Char
import Data.List

import PrismaSchemaRaw (Schema (..), databaseUrl, enumTypes, models,
                        DatabaseURL (..), 
                        Model (..), modelName, fields,
                        EnumType (..), enumName, values,
                        Field (..), fieldName, fieldType, attributes,
                        FieldType (..), 
                        Attribute (..),
                        Expression (..),
                        IntExpression (..),
                        StringExpression (..),
                        DateTimeExpression (..))

import Parser (Parser, parse, string, space, digit, choice, satisfy, eof)

wsP :: Parser a -> Parser a
wsP p = p <* many space

mapP :: String -> a -> Parser a
mapP s v = const v <$> string s

nameP :: Parser String 
nameP = do
  first <- satisfy (\x -> isAlpha x || x == '_')
  rest <- many $ satisfy (\x -> isAlpha x || isDigit x || x == '_')
  return (first : rest)

stringP = string "\"" >> many (satisfy (/= '"')) <* string "\""

listP :: String -> String -> Parser a -> Parser [a]
listP l r p = wsP $ do
  _ <- string l
  itemsP p where
    itemsP :: Parser a -> Parser [a]
    itemsP p = choice [
      mapP r [],
      singleP p,
      moreP p >>= \x -> (x :) <$> itemsP p]
    singleP :: Parser a -> Parser [a]
    singleP p = do
      x <- p
      _ <- string r
      return [x]
    moreP :: Parser a -> Parser a
    moreP p = do
      x <- p
      _ <- string ", "
      return x

schemaP :: Parser Schema
schemaP = go (DirectURL "") [] []
  where
    go :: DatabaseURL -> [EnumType] -> [Model] -> Parser Schema
    go url enums models = choice [
      datasourceP >>= \url' -> go url' enums models,
      enumP >>= \e -> go url (e:enums) models,
      modelP >>= \m -> go url enums (m:models),
      genP >> go url enums models,
      eof >> return (Schema { 
        databaseUrl = url, 
        enumTypes = reverse enums, 
        models = reverse models}) ]
    genP =
      wsP (string "generator") >> wsP nameP >> wsP (string "{") >> 
      many genFieldP >> wsP (string "}")
    genFieldP = wsP $ choice [strP "provider", strP "output", strP "engineType",
        lstP "previewFeatures", lstP "binaryTargets"]
    strP x = wsP (string x) >> wsP (string "=") >> wsP stringP >> return ()
    lstP x = wsP (string x) >> wsP (string "=") >> wsP (listP "[" "]" stringP) >> return ()

datasourceP :: Parser DatabaseURL
datasourceP = wsP $ do
  _ <- wsP (string "datasource") >> wsP nameP <* wsP (string "{")
  parseDS (DirectURL "") <* string "}"  where
    parseDS :: DatabaseURL -> Parser DatabaseURL
    parseDS url = wsP $ choice [
        notUrl "provider", notUrl "directUrl", notUrl "shadowDatabaseUrl",
        isUrl, string "" >> return url] where
      notUrl x = wsP (string x) >> wsP (string "=") >> wsP stringP >> parseDS url
    isUrl = do
      _ <- wsP (string "url") >> wsP (string "=")
      url' <- wsP urlP
      parseDS url'
    urlP = choice [
      EnvironmentVariable <$> (wsP (string "env(") >> wsP stringP <* string ")"),
      DirectURL <$> stringP]

modelP :: Parser Model
modelP = wsP $ do
  _ <- string "model" >> many space
  name <- wsP nameP <* string "{" <* many space
  fields <- some fieldP <* string "}"
  return $ Model { modelName = name, fields = fields }

enumP :: Parser EnumType
enumP = wsP $ do
  _ <- string "enum" >> many space
  name <- wsP nameP <* string "{" <* many space
  values <- some (wsP nameP) <* string "}"
  return $ EnumDefinition { enumName = name, values = values }

fieldP :: Parser Field
fieldP = wsP $ do
  name <- wsP nameP
  typ <- fieldTypeP
  attrs <- many attributeP
  return $ Field {fieldName = name, fieldType = typ, attributes = attrs}

fieldTypeP :: Parser FieldType
fieldTypeP = wsP $ do
  t <- primitiveP
  modifyP t
  where
    primitiveP = choice [
      mapP "Int" IntField,
      mapP "String" StringField,
      mapP "Boolean" BooleanField,
      mapP "Float" FloatField,
      mapP "Decimal" DecimalField,
      mapP "Json" JsonField,
      mapP "Bytes" BytesField,
      mapP "DateTime" DateTimeField,
      modelNameP]
    modelNameP = ModelField <$> nameP
    modifyP t = choice [
      string "[]" >> modifyP (ListOf t),
      string "?" >> modifyP (OptionalField t),
      string "" >> return t] 

attributeP :: Parser Attribute
attributeP = wsP $ choice 
    [idP, defaultP, uniqueP, relationP, mapAttrP, updatedP, ignoreP]
  where
    idP = mapP "@id" IDAttribute
    defaultP = do
      _ <- string "@default("
      e <- expressionP
      _ <- string ")"
      return $ DefaultAttribute e
    uniqueP = mapP "@unique" UniqueAttribute
    relationP = do
      _ <- string "@relation"
      args <- listP "(" ")" argP
      return $ case args of
        [("fields", fields), ("references", refs)] -> RelationAttribute fields refs
        [("references", refs), ("fields", fields)] -> RelationAttribute fields refs
        [("fields", fields)] -> RelationAttribute fields []
        [("references", refs)] -> RelationAttribute [] refs
        _ -> RelationAttribute [] []
    argP = do
      name <- choice [string "fields", string "references"]
      _ <- string ": "
      items <- listP "[" "]" nameP
      return (name, items)
    mapAttrP = do
      _ <- string "@map(\""
      s <- many (satisfy (/= '"'))
      _ <- string"\")"
      return $ MapAttribute s
    updatedP = mapP "@updatedAt" UpdatedAtAttribute
    ignoreP = mapP "@ignore" IgnoreAttribute

expressionP :: Parser Expression -- don't need individual parsers for each expression type
expressionP = wsP $ choice [
  mapP "autoincrement()" (IntExpression AutoIncrementExpression),
  mapP "cuid()" (StringExpression CuidExpression),
  mapP "uuid()" (StringExpression UuidExpression),
  mapP "now()" (DateTimeExpression NowExpression),
  (IntExpression . IntLiteralExpression) <$> intP] where
    intP = read <$> ((++) <$> string "-" <*> some digit <|> some digit)
