module Client (
  User.User, User.getId, User.getEmail, User.getName, User.getDob,
  User.Id, User.Email, User.Name, User.Dob,
  User.QId, User.QEmail, User.QName, User.QDob,
  User.UId, User.UEmail, User.UName, User.UDob,
  User.create, User.createMany,
  User.findFirst, User.findMany, User.findUnique,
  User.updateFirst, User.updateMany, User.updateUnique,
  User.deleteFirst, User.deleteMany, User.deleteUnique
) where

import Client.User
