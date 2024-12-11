{-# LANGUAGE FlexibleInstances #-}
module PrismaSchemaRaw where

-- used for printing schemas
-- don't want to use show because we want to see what exactly is generated when testing
class ToString a where
  toString :: a -> String

-- Prisma schema as it exists in the schema.prisma file

data Schema = Schema {
    databaseUrl :: DatabaseURL,
    enumTypes :: [EnumType],
    models :: [Model]
} deriving (Show, Eq)

instance ToString Schema where
  toString s =
    "datasource db {\n  url = " ++ toString (databaseUrl s) ++ "\n}\n\n" ++
    showSep "\n\n" (models s) ++
    showSep "\n\n" (enumTypes s)

data DatabaseURL = 
  DirectURL String
  | EnvironmentVariable String
  deriving (Show, Eq)

instance ToString DatabaseURL where
  toString (DirectURL url) = "\"" ++ url ++ "\""
  toString (EnvironmentVariable var) = "env(\"" ++ var ++ "\")"

data EnumType = EnumDefinition {
    enumName :: String,
    values :: [String]
} deriving (Show, Eq)

instance ToString EnumType where
  toString e =
    "enum " ++ enumName e ++ " {\n" ++ showVals (values e) where
      showVals [] = "}"
      showVals (v:vs) = "  " ++ v ++ "\n" ++ showVals vs

data Model = Model {
    modelName :: String,
    fields :: [Field]
} deriving (Show, Eq)

instance ToString Model where
  toString m =
    "model " ++ modelName m ++ " {\n" ++
    showSep "\n" (map (("  " ++) . toString) (fields m)) ++ 
    "}"

data Field = Field {
    fieldName :: String,
    fieldType :: FieldType,
    attributes :: [Attribute]
 } deriving (Show, Eq)

instance ToString Field where
  toString f = 
    fieldName f ++ " " ++ 
    toString (fieldType f) ++ " " ++ 
    showSep " " (attributes f)

data FieldType = 
  IntField
  | StringField
  | BooleanField
  | FloatField
  | DecimalField
  | DateTimeField
  | JsonField
  | BytesField
  | ListOf FieldType
  | OptionalField FieldType
  | ModelField String -- Name of the model this field relates to
--  | EnumField String -- Name of the enum type -- for the raw, should get mapped to Model
-- since there's no way for the parser to know if it's a model or enum without knowledge of the whole schema
  deriving (Show, Eq)

instance ToString FieldType where
  toString IntField = "Int"
  toString StringField = "String"
  toString BooleanField = "Boolean"
  toString FloatField = "Float"
  toString DecimalField = "Decimal"
  toString DateTimeField = "DateTime"
  toString JsonField = "Json"
  toString BytesField = "Bytes"
  toString (ListOf t) = toString t ++ "[]"
  toString (OptionalField t) = toString t ++ "?"
  toString (ModelField x) = x

data Attribute = 
    IDAttribute
  | DefaultAttribute Expression
  | UniqueAttribute
  | RelationAttribute [String] [String] -- fields, references
  | MapAttribute String  --The database column to use
  | UpdatedAtAttribute
  | IgnoreAttribute
  deriving (Show, Eq)

instance ToString Attribute where
  toString IDAttribute = "@id"
  toString (DefaultAttribute exp) = "@default(" ++ toString exp ++ ")"
  toString UniqueAttribute = "@unique"
  toString (RelationAttribute fields refs) = "@relation(fields: " ++ toString fields ++ ", references: " ++ toString refs ++ ")"
  toString (MapAttribute x) = "@map(\"" ++ x ++ "\")"
  toString UpdatedAtAttribute = "@updatedAt"
  toString IgnoreAttribute = "@ignore"

data IntExpression = 
    IntLiteralExpression Int
  | AutoIncrementExpression
  deriving (Show, Eq)

data StringExpression = 
    StringLiteralExpression String
  | CuidExpression
  | UuidExpression
  deriving (Show, Eq)

data DateTimeExpression = 
  NowExpression
  deriving (Show, Eq)

data Expression = 
    IntExpression IntExpression
  | StringExpression StringExpression
  | DateTimeExpression DateTimeExpression
  deriving (Show, Eq)

instance ToString Expression where
  toString (IntExpression (IntLiteralExpression x)) = toString x
  toString (IntExpression AutoIncrementExpression) = "autoincrement()"
  toString (StringExpression (StringLiteralExpression x)) = show x
  toString (StringExpression UuidExpression) = "uuid()"
  toString (StringExpression CuidExpression) = "cuid()"
  toString (DateTimeExpression NowExpression) = "now()"

showSep :: ToString a => String -> [a] -> String
showSep sep [] = ""
showSep sep (x : xs) = toString x ++ sep ++ showSep sep xs

instance ToString String where
  toString s = s

instance ToString Int where
  toString = show

instance ToString [String] where
  toString xs = "[" ++ go xs ++ "]" where
    go [] = ""
    go [x] = toString x
    go (x:xs) = toString x ++ ", " ++ go xs