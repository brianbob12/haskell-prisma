module User (
  User (..),
  Value (..), Query (..), Update (..),
  create, createMany,
  findFirst, findMany, findUnique,
  updateMany, updateUnique,
  deleteMany, deleteUnique
) where

import qualified ClientInternal as CI
import ClientInternal (Result (..))
import qualified Database.SQLite.Simple as SQL
import Data.String (fromString)
import System.Environment (getEnv)
import Control.Exception (try, SomeException)

data User = User { 
  getUserId :: Int,
  getUserEmail :: String,
  getUserName :: String,
  getUserDob :: Int
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
dbUrl = return "testClient/db.db"

table :: String
table = "User"

type RecordType = User
type ResultTuple = (Int, String, String, Int)

singleResult :: [ResultTuple] -> Result User
singleResult ((id, email, name, dob) : _) = OK $ User id email name dob 
singleResult _ = SqlError "No row found"


mapResults :: [ResultTuple] -> [User]
mapResults = map (\(id, email, name, dob) -> User id email name dob )

create :: [Value] -> IO (Result ())
create values = do
  url <- dbUrl
  go (toClientValues values) url where
    go (SqlError x) _ = return $ SqlError x
    go (OK vals) url = do
      let sqlQuery = CI.create table vals
      result <- try $ do 
        conn <- SQL.open url
        SQL.execute_ conn (SQL.Query (fromString sqlQuery))
        SQL.close conn
      case result of
        Left ex  -> return $ SqlError (show (ex :: SomeException))
        Right () -> return $ OK ()

createMany :: [[Value]] -> IO (Result ())
createMany rows = do
  url <- dbUrl
  let rows' = toClientValues <$> rows
  if any isError rows' then return $ SqlError "Missing field(s)"
  else go (toRows rows') url where
    go rs url = do
      let sqlQuery = CI.createMany table rs
      result <- try $ do
        conn <- SQL.open url
        SQL.execute_ conn (SQL.Query (fromString sqlQuery))
        SQL.close conn
      case result of
        Left ex -> return $ SqlError (show (ex :: SomeException))
        Right () -> return $ OK ()
    isError (SqlError _) = True; isError _ = False
    toRows (OK x : xs) = x : toRows xs
    toRows _ = []

findFirst :: [Query] -> IO (Result User)
findFirst queries = do
  url <- dbUrl
  let sqlQuery = CI.findFirst table (toClientQueries queries)  
  result <- try $ do
    conn <- SQL.open url
    results <- SQL.query_ conn (SQL.Query (fromString sqlQuery)) :: IO [ResultTuple]
    SQL.close conn
    return $ singleResult results
  case result of
    Left ex -> return $ SqlError (show (ex :: SomeException))
    Right x -> return x

findMany :: [Query] -> IO (Result [User])
findMany queries = do
  url <- dbUrl
  let sqlQuery = CI.findMany table (toClientQueries queries)
  result <- try $ do
    conn <- SQL.open url
    results <- SQL.query_ conn (SQL.Query (fromString sqlQuery)) :: IO [ResultTuple]
    SQL.close conn
    return $ mapResults results
  case result of
    Left ex -> return $ SqlError (show (ex :: SomeException))
    Right x -> return $ OK x

findUnique :: [Value] -> IO (Result User)
findUnique values = do
  url <- dbUrl
  let sqlQuery = CI.findFirst table (map convertValue values)
  result <- try $ do
    conn <- SQL.open url
    results <- SQL.query_ conn (SQL.Query (fromString sqlQuery)) :: IO [ResultTuple]
    SQL.close conn
    return $ singleResult results
  case result of
    Left ex -> return $ SqlError (show (ex :: SomeException))
    Right x -> return x

updateMany :: [Query] -> [Update] -> IO (Result ())
updateMany queries updates = do
  url <- dbUrl
  let sqlQuery = CI.updateMany table (toClientQueries queries) (map toClientUpdate updates)
  result <- try $ do
    conn <- SQL.open url
    SQL.execute_ conn (SQL.Query (fromString sqlQuery))
    SQL.close conn
  case result of
    Left ex -> return $ SqlError (show (ex :: SomeException))
    Right () -> return $ OK ()

updateUnique :: [Value] -> [Update] -> IO (Result ())
updateUnique values updates = do
  url <- dbUrl
  let sqlQuery = CI.updateUnique table (map convertValue values) (map toClientUpdate updates)
  result <- try $ do
    conn <- SQL.open url
    SQL.execute_ conn (SQL.Query (fromString sqlQuery))
    SQL.close conn
  case result of 
    Left ex -> return $ SqlError (show (ex :: SomeException))
    Right () -> return $ OK ()

deleteMany :: [Query] -> IO (Result ())
deleteMany queries = do
  url <- dbUrl
  let sqlQuery = CI.deleteMany table (toClientQueries queries)
  result <- try $ do
    conn <- SQL.open url
    SQL.execute_ conn (SQL.Query (fromString sqlQuery))
    SQL.close conn
  case result of 
    Left ex -> return $ SqlError (show (ex :: SomeException))
    Right () -> return $ OK ()

deleteUnique :: [Value] -> IO (Result ())
deleteUnique values = do
  url <- dbUrl
  let sqlQuery = CI.deleteUnique table (map convertValue values)
  result <- try $ do
    conn <- SQL.open url
    SQL.execute_ conn (SQL.Query (fromString sqlQuery))
    SQL.close conn
  case result of
    Left ex -> return $ SqlError (show (ex :: SomeException))
    Right () -> return $ OK ()

toClientValues :: [Value] -> Result [CI.Value]
toClientValues values = 
  if allThere values then OK $ map convertValue values
    else SqlError "Missing field(s)"
  where
    convertValue (Id x) = CI.IntVal "id" x
    convertValue (Email x) = CI.StringVal "email" x
    convertValue (Name x) = CI.StringVal "name" x
    convertValue (Dob x) = CI.IntVal "dob" x

    allThere = aux [isEmail, isName, isDob]
    aux [] _ = True; aux (f:fs) vs = any f vs && aux fs vs
    isEmail (Email _) = True; isEmail _ = False
    isName (Name _) = True; isName _ = False
    isDob (Dob _) = True; isDob _ = False



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


