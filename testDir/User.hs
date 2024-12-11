module User (
  User (..),
  Value (..), Query (..), Update (..),
  create, createMany,
  findFirst, findMany, findUnique,
  updateMany, updateUnique,
  deleteMany, deleteUnique
) where

import qualified ClientInternal as CI
import qualified Database.SQLite.Simple as SQL
import Data.String (fromString)
import System.Environment (getEnv)

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
  deriving Show


data Query =
    Or [Query]
  | Not Query
  | QId CI.IntQuery
  | QEmail CI.StringQuery
  | QName CI.StringQuery
  | QDob CI.IntQuery
  deriving Show


data Update =
    UId CI.IntUpdate
  | UEmail CI.StringUpdate
  | UName CI.StringUpdate
  | UDob CI.IntUpdate
  deriving Show


dbUrl :: IO String
dbUrl = return "testDir/db.db"

table :: String
table = "User"

type RecordType = User
type ResultTuple = (Int, String, String, Int)

singleResult :: [ResultTuple] -> Maybe User
singleResult ((id, email, name, dob) : _) = Just $ User id email name dob 
singleResult _ = Nothing


mapResults :: [ResultTuple] -> [User]
mapResults = map (\(id, email, name, dob) -> User id email name dob )


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

findFirst :: [Query] -> IO (Maybe RecordType)
findFirst queries = do
  url <- dbUrl
  conn <- SQL.open url
  let sqlQuery = CI.findFirst table (toClientQueries queries)
  results <- SQL.query_ conn (SQL.Query (fromString sqlQuery)) :: IO [ResultTuple]
  SQL.close conn
  return $ singleResult results

findMany :: [Query] -> IO [RecordType]
findMany queries = do
  url <- dbUrl
  conn <- SQL.open url
  let sqlQuery = CI.findMany table (toClientQueries queries)
  results <- SQL.query_ conn (SQL.Query (fromString sqlQuery)) :: IO [ResultTuple]
  SQL.close conn
  return $ mapResults results

findUnique :: [Value] -> IO (Maybe RecordType)
findUnique values = do
  url <- dbUrl
  conn <- SQL.open url
  let sqlQuery = CI.findFirst table (map convertValue values)
  results <- SQL.query_ conn (SQL.Query (fromString sqlQuery)) :: IO [ResultTuple]
  SQL.close conn
  return $ singleResult results

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
    convertQuery (QId q) = CI.QInt "id" q
    convertQuery (QEmail q) = CI.QString "email" q
    convertQuery (QName q) = CI.QString "name" q
    convertQuery (QDob q) = CI.QInt "dob" q



convertValue :: Value -> CI.Query
convertValue (Id x) = CI.QInt "id" (CI.IntEquals x)
convertValue (Email x) = CI.QString "email" (CI.StringEquals x)
convertValue (Name x) = CI.QString "name" (CI.StringEquals x)
convertValue (Dob x) = CI.QInt "dob" (CI.IntEquals x)



toClientUpdate :: Update -> CI.Update
toClientUpdate (UId u) = CI.UInt "id" u
toClientUpdate (UEmail u) = CI.UString "email" u
toClientUpdate (UName u) = CI.UString "name" u
toClientUpdate (UDob u) = CI.UInt "dob" u


