module Client (
  -- query constructors from ClientInternal
  IntQuery (..), StringQuery (..), DoubleQuery (..), BytesQuery (..),
  IntUpdate (..), StringUpdate (..), DoubleUpdate (..), BytesUpdate (..),
  -- interfact for Client.User
  User.User (..), 
  User.Value (..), User.Query (..), User.Update (..),
  User.create, User.createMany,
  User.findFirst, User.findMany, User.findUnique,
  User.updateMany, User.updateUnique,
  User.deleteMany, User.deleteUnique
) where

import ClientInternal
import qualified User
