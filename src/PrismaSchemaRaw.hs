module PrismaSchemaRaw (
  Schema,
  Model,
  EnumType,
  DatabaseURL,
  Field,
  FieldType,
  Attribute,
  Expression,
  IntExpression,
  StringExpression,
  DateTimeExpression
) where

-- Prisma schema as it exists in the schema.prisma file

data Schema = Schema {
    databaseUrl :: DatabaseURL,
    enumTypes :: [EnumType],
    models :: [Model]
} deriving (Show, Eq)

data Model = Model {
    modelName :: String,
    fields :: [Field]
} deriving (Show, Eq)

data EnumType = EnumDefinition {
    enumName :: String,
    values :: [String]
} deriving (Show, Eq)

data DatabaseURL = 
  DirectURL String
  | EnvironmentVariable String
  deriving (Show, Eq)

data Field = Field {
    fieldName :: String,
    fieldType :: FieldType,
    attributes :: [Attribute]
 } deriving (Show, Eq)

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
  | EnumField String -- Name of the enum type
  deriving (Show, Eq)

data Attribute = 
  IDAttribute
  | DefaultAttribute Expression
  | UniqueAttribute
  | RelationAttribute String [String] [String] -- Model, fields, references
  | MapAttribute String  --The database column to use
  | UpdatedAtAttribute
  | IgnoreAttribute
  deriving (Show, Eq)

data Expression = 
  IntExpression IntExpression
  | StringExpression StringExpression
  | DateTimeExpression DateTimeExpression
  deriving (Show, Eq)

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