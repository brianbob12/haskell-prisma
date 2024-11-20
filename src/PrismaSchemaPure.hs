module PrismaSchemaPure where

-- Prisma schema as it pertains to the client

data Schema = Schema {
    databaseUrl :: DatabaseURL,
    enumTypes :: [EnumType],
    models :: [Model]
} deriving (Show, Eq)

data Model = Model {
    modelName :: String,
    fields :: [Field]
} deriving (Show, Eq)

data EnumType = EnumType {
    enumName :: String,
    values :: [String]
} deriving (Show, Eq)

data Field = Field {
    fieldName :: String,
    fieldType :: FieldType,
    attributes :: [Attribute]
} deriving (Show, Eq)

data DatabaseURL = 
  DirectURL String
  | EnvironmentVariable String
  deriving (Show, Eq)

data FieldType = 
  IntField -- Datetime gets mapped to int
  | TextField 
  | RealField -- Decimal and float get mapped to double
  | DecimalField
  | BlobField
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

