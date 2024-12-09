module Client.User (
  User (User), getId, getEmail, getName, getDob,
  Value (..), Query (..), Update (..),
  create, createMany,
  findFirst, findMany, findUnique,
  updateMany, updateUnique,
  deleteMany, deleteUnique
) where

import qualified ClientInternal as CI
import qualified Database.SQLite.Simple as SQL
import Data.String (fromString)

data User = User {
  getId :: Int,
  getEmail :: String,
  getName :: String,
  getDob :: Int
} deriving Show

data Value =
    Id Int
  | Email String
  | Name String
  | Dob Int

-- AND by default
data Query =
    QId CI.IntQuery
  | QEmail CI.StringQuery
  | QName CI.StringQuery
  | QDob CI.IntQuery
  | Or [Query]
  | Not Query

data Update =
    UId CI.IntUpdate
  | UEmail CI.StringUpdate
  | UName CI.StringUpdate
  | UDob CI.IntUpdate

dbUrl :: IO String
dbUrl = return "db"

table :: String
table = "Users"

type ResultTuple = (Int, String, String, Int)

singleResult :: [ResultTuple] -> Maybe User
singleResult ((id, email, name, dob) : _) = Just $ User id email name dob
singleResult _ = Nothing

mapResults :: [ResultTuple] -> [User]
mapResults = map (\(id, email, name, dob) -> User id email name dob)

create :: [Value] -> IO ()
create values = do
  url <- dbUrl
  conn <- SQL.open url
  let sqlQuery = CI.create table (toClientValues values)
  SQL.execute_ conn (SQL.Query (fromString sqlQuery))
  SQL.close conn

createMany rows = do
  url <- dbUrl
  conn <- SQL.open url
  let sqlQuery = CI.createMany table (map toClientValues rows)
  SQL.execute_ conn (SQL.Query (fromString sqlQuery))
  SQL.close conn

findFirst :: [Query] -> IO (Maybe User)
findFirst queries = do
  url <- dbUrl
  conn <- SQL.open url
  let sqlQuery = CI.findFirst table (toClientQueries queries)
  results <- SQL.query_ conn (SQL.Query (fromString sqlQuery)) :: IO [ResultTuple]
  SQL.close conn
  return $ singleResult results

findMany :: [Query] -> IO [User]
findMany queries = do
  url <- dbUrl
  conn <- SQL.open url
  let sqlQuery = CI.findMany table (toClientQueries queries)
  results <- SQL.query_ conn (SQL.Query (fromString sqlQuery)) :: IO [ResultTuple]
  SQL.close conn
  return $ mapResults results

findUnique :: [Value] -> IO (Maybe User)
findUnique values = do
  url <- dbUrl
  conn <- SQL.open url
  let sqlQuery = CI.findFirst table (map convertValue values)
  results <- SQL.query_ conn (SQL.Query (fromString sqlQuery)) :: IO [ResultTuple]
  SQL.close conn
  return $ singleResult results

{-updateFirst :: [Query] -> [Update] -> IO ()
updateFirst queries updates = do
  url <- dbUrl
  conn <- SQL.open url
  let sqlQuery = CI.updateFirst table (toClientQueries queries) (map toClientUpdate updates)
  SQL.execute_ conn (SQL.Query (fromString sqlQuery))
  SQL.close conn -}

updateMany :: [Query] -> [Update] -> IO ()
updateMany queries updates = do
  url <- dbUrl
  conn <- SQL.open url
  let sqlQuery = CI.updateMany table (toClientQueries queries) (map toClientUpdate updates)
  SQL.execute_ conn (SQL.Query (fromString sqlQuery))
  SQL.close conn

updateUnique :: [Value] -> [Update] -> IO ()
updateUnique values updates = do
  url <- dbUrl
  conn <- SQL.open url
  let sqlQuery = CI.updateUnique table (map convertValue values) (map toClientUpdate updates)
  SQL.execute_ conn (SQL.Query (fromString sqlQuery))
  SQL.close conn

{-deleteFirst :: [Query] -> IO ()
deleteFirst queries = do
  url <- dbUrl
  conn <- SQL.open url
  let sqlQuery = CI.deleteFirst table (toClientQueries queries)
  SQL.execute_ conn (SQL.Query (fromString sqlQuery))
  SQL.close conn -}

deleteMany :: [Query] -> IO ()
deleteMany queries = do
  url <- dbUrl
  conn <- SQL.open url
  let sqlQuery = CI.deleteMany table (toClientQueries queries)
  SQL.execute_ conn (SQL.Query (fromString sqlQuery))
  SQL.close conn

deleteUnique :: [Value] -> IO ()
deleteUnique values = do
  url <- dbUrl
  conn <- SQL.open url
  let sqlQuery = CI.deleteUnique table (map convertValue values)
  SQL.execute_ conn (SQL.Query (fromString sqlQuery))
  SQL.close conn

toClientValues :: [Value] -> [CI.Value]
toClientValues values = map convertValue values
  where
    convertValue (Id x) = CI.IntVal "id" x
    convertValue (Email x) = CI.StringVal "email" x
    convertValue (Name x) = CI.StringVal "name" x
    convertValue (Dob x) = CI.IntVal "dob" x

toClientQueries :: [Query] -> [CI.Query]
toClientQueries = map convertQuery
  where
    convertQuery (QId intQuery) = CI.QInt "id" intQuery
    convertQuery (QEmail strQuery) = CI.QString "email" strQuery
    convertQuery (QName strQuery) = CI.QString "name" strQuery
    convertQuery (QDob intQuery) = CI.QInt "dob" intQuery
    convertQuery (Or queries) = CI.Or (toClientQueries queries)
    convertQuery (Not query) = CI.Not (convertQuery query)

convertValue :: Value -> CI.Query
convertValue (Id x) = CI.QInt "id" (CI.IntEquals x)
convertValue (Email x) = CI.QString "email" (CI.StringEquals x)
convertValue (Name x) = CI.QString "name" (CI.StringEquals x)
convertValue (Dob x) = CI.QInt "dob" (CI.IntEquals x)

toClientUpdate :: Update -> CI.Update
toClientUpdate (UId intUpdate) = CI.UInt "id" intUpdate
toClientUpdate (UEmail strUpdate) = CI.UString "email" strUpdate
toClientUpdate (UName strUpdate) = CI.UString "name" strUpdate
toClientUpdate (UDob intUpdate) = CI.UInt "dob" intUpdate
