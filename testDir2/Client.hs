module Client (
  -- query constructors from ClientInternal
  IntQuery (..), StringQuery (..), DoubleQuery (..), BytesQuery (..),
  IntUpdate (..), StringUpdate (..), DoubleUpdate (..), BytesUpdate (..),
  

  
  --Exports for User
  User.User (..),
  User_Value (..), User_Query (..), User_Update (..),
  User_create, User_createMany,
  User_findFirst, User_findMany, User_findUnique,
  User_updateMany, User_updateUnique,
  User_deleteMany, User_deleteUnique

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

