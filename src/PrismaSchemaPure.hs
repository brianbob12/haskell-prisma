module PrismaSchemaPure where

-- Prisma schema as it's pertains to the client

data Schema = Schema {
    databaseUrl :: String,
    enumTypes :: [EnumType],
    models :: [Model]
} deriving (Show, Eq)

data DatabaseURL = 
  DirectURL String
  | EnvironmentVariable String
  deriving (Show, Eq)

data EnumDefinition = EnumDefinition {
    name :: String,
    values :: [String]
} deriving (Show, Eq)

data Model = Model {
    name :: String,
    fields :: [Field]
} deriving (Show, Eq)

data FieldType = 
  IntField -- Datetime gets mapped to int
  | StringField -- Json gets mapped to string
  | BooleanField
  | DoubleField -- Decimal and float get mapped to double
  | BytesField
  | ListOf FieldType
  | OptionalField FieldType
  | EnumField String -- Name of the enum type
  deriving (Show, Eq)

--Ignore and unique are removed
data Attribute = 
  IDAttribute
  | DefaultAttribute Expression
  | RelationAttribute String String -- Model, field
  | MapAttribute String --The database column to use
  | UpdatedAtAttribute
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

data Expression = 
  IntExpression IntExpression
  | StringExpression StringExpression
  | DateTimeExpression DateTimeExpression
  deriving (Show, Eq)

data Field = Field {
    name :: String,
    fieldType :: FieldType,
    attributes :: [Attribute],
} deriving (Show, Eq)

