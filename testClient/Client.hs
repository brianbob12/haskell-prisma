module Client (
  -- query constructors from ClientInternal
  IntQuery (..), StringQuery (..), DoubleQuery (..), BytesQuery (..),
  IntUpdate (..), StringUpdate (..), DoubleUpdate (..), BytesUpdate (..),
  -- Result type from ClientInternal
  Result (..),
  

  
  --Exports for User
  User.User (..),
  User_Value (..), User_Query (..), User_Update (..),
  user_create, user_createMany,
  user_findFirst, user_findMany, user_findUnique,
  user_updateMany, user_updateUnique,
  user_deleteMany, user_deleteUnique

) where

import ClientInternal

import qualified User


-- module bindings for User
type User_Value = User.Value
type User_Query = User.Query
type User_Update = User.Update

user_create = User.create
user_createMany = User.create
user_findFirst = User.findFirst
user_findMany = User.findMany
user_findUnique = User.findUnique
user_updateMany = User.updateMany
user_updateUnique = User.updateUnique
user_deleteMany = User.deleteMany
user_deleteUnique = User.deleteUnique

