module Client.User (
  User (User), getId, getEmail, getName, getDob,
  Value (..), Query (..), Update (..),
  create, createMany,
  findFirst, findMany, findUnique,
  updateFirst, updateMany, updateUnique,
  deleteFirst, deleteMany, deleteUnique
) where

import ClientInternal

data User = User {
  getId :: Int,
  getEmail :: String,
  getName :: String,
  getDob :: Int
}

data Value =
    Id Int
  | Email String
  | Name String
  | Dob Int

-- AND by default
data Query =
    QId IntQuery
  | QEmail StringQuery
  | QName StringQuery
  | QDob IntQuery
  | Or [Query]

data Update =
    UId IntUpdate
  | UEmail StringUpdate
  | UName StringUpdate
  | UDob IntUpdate

create :: [Value] -> IO ()
create = undefined

createMany :: [[Value]] -> IO ()
createMany = undefined

findFirst :: [Query] -> IO (Maybe User)
findFirst = undefined

findMany :: [Query] -> IO [User]
findMany = undefined

findUnique :: [Value] -> IO (Maybe User)
findUnique = undefined

updateFirst :: [Query] -> [Update] -> IO ()
updateFirst = undefined

updateMany :: IO ()
updateMany = undefined

updateUnique :: [Value] -> [Update] -> IO ()
updateUnique = undefined

deleteFirst :: [Query] -> IO ()
deleteFirst = undefined

deleteMany :: [Query] -> IO ()
deleteMany = undefined

deleteUnique :: [Value] -> IO ()
deleteUnique = undefined
