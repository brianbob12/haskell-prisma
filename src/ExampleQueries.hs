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
    userReadId :: Int,
    userReadEmail :: String,
    userRoleRole :: Role
} deriving (Show, Eq)

data UserCreate = UserCreate {
    userCreateId :: Maybe Int,
    userCreateEmail :: String,
    userCreateRole :: Role
} deriving (Show, Eq)

data UserUpdate = UserUpdate {
    userUpdateEmail :: Maybe String,
    userUpdateRole :: Maybe Role
} deriving (Show, Eq)

-- Create

createUser :: UserCreate -> IO (Maybe UserRead)

data CreateManyUserArgs = {
    userCreateManyData :: [UserCreate],
    userCreateManySkipDuplicates :: Bool
} deriving (Show, Eq)

data CreateManyUserResult = {
  createManyUserCount :: Int
} deriving (Show, Eq)

createManyUser :: CreateManyUserArgs -> IO CreateManyUserResult

-- Read

data UserQuery = {
    userQueryId :: IntQuery,
    userQueryEmail :: StringQuery,
    userQueryRole :: RoleQuery
} deriving (Show, Eq)

data UserOrderBy =
  OrderByID OrderDirection
  | OrderByEmail OrderDirection
  | OrderByRole OrderDirection
  deriving (Show, Eq)

data FindUniqueUserArgs = {
    findUniqueUserWhere :: UserQuery
} deriving (Show, Eq)

type FindUniqueUserResult = Maybe UserRead

findUniqueUser :: FindUniqueUserArgs -> IO FindUniqueUserResult

data FindFirstUserArgs = {
    findFirstUserWhere :: UserQuery,
    findFirstUserOrderBy :: [UserOrderBy]
} deriving (Show, Eq)
type FindFirstUserResult = Maybe UserRead
findFirstUser :: FindFirstUserArgs -> IO FindFirstUserResult

data FindManyUserArgs = {
    findManyUserWhere :: UserQuery,
    findManyUserOrderBy :: [UserOrderBy],
    findManyUserCursor :: Maybe UserRead,
    findManyUserTake :: Maybe Int,
    findManyUserSkip :: Maybe Int
} deriving (Show, Eq)

type FindManyUserResult = [UserRead]

findManyUser :: FindManyUserArgs -> IO FindManyUserResult
