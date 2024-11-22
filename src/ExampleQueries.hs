module ExampleQueries where

import PrismaSchemaPure
import QueryTypes

data OrderDirection = Asc | Desc deriving (Show, Eq)

exampleSchema1 :: Schema
exampleSchema1 = Schema {
    databaseUrl = DirectURL "postgresql://user:password@localhost:5432/mydb",
    enumTypes = [
        EnumType {
            enumName = "Role",
            values = ["USER", "ADMIN"]
        }
    ],
    models = [
        Model {
            modelName = "User",
            fields = [
                Field {
                    fieldName = "id",
                    fieldType = IntField,
                    attributes = [IDAttribute, DefaultAttribute (AutoIncrementExpression)]
                },
                Field {
                    fieldName = "email",
                    fieldType = TextField,
                    attributes = []
                },
                Field {
                    fieldName = "role",
                    fieldType = EnumField "Role",
                    attributes = []
                }
            ]
        }
    ]
}

data Role = User | Admin deriving (Show, Eq)

data RoleQuery = 
  RoleNothing
  | RoleEqual Role
  | RoleNot RoleQuery
  | RoleAnd RoleQuery RoleQuery
  | RoleOr RoleQuery RoleQuery
  deriving (Show, Eq)

data UserRead = UserRead {
    id :: Int,
    email :: String,
    role :: Role
} deriving (Show, Eq)

data UserCreate = UserCreate {
    id :: Maybe Int,
    email :: String,
    role :: Role
} deriving (Show, Eq)

data UserUpdate = UserUpdate {
    email :: Maybe String,
    role :: Maybe Role
} deriving (Show, Eq)

-- Create

createUser :: UserCreate -> IO (Maybe UserRead)

type CreateManyUserArgs = {
    data :: [UserCreate],
    skipDuplicates :: Bool
}
type CreateManyUserResult = {
  count :: Int
}
createManyUser :: CreateManyUserArgs -> IO CreateManyUserResult

-- Read

data UserQuery = {
    id :: IntQuery,
    email :: StringQuery,
    role :: RoleQuery
} deriving (Show, Eq)

data UserOrderBy =
  OrderByID OrderDirection
  | OrderByEmail OrderDirection
  | OrderByRole OrderDirection
  deriving (Show, Eq)

type FindUniqueUserArgs = {
    where :: UserQuery,
}
type FindUniqueUserResult = Maybe UserRead
findUniqueUser :: FindUniqueUserArgs -> IO FindUniqueUserResult

data FindFirstUserArgs = {
    where :: UserQuery,
    orderBy :: [UserOrderBy]
} deriving (Show, Eq)
type FindFirstUserResult = Maybe UserRead
findFirstUser :: FindFirstUserArgs -> IO FindFirstUserResult

data FindManyUserArgs = {
    where :: UserQuery,
    orderBy :: [UserOrderBy],
    cursor :: Maybe UserRead,
    take :: Maybe Int,
    skip :: Maybe Int
} deriving (Show, Eq)
type FindManyUserResult = [UserRead]
findManyUser :: FindManyUserArgs -> IO FindManyUserResult
