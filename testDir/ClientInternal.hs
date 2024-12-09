module ClientInternal (
  IntQuery (..), StringQuery (..), DoubleQuery (..), BytesQuery (..), Query (..),
  IntUpdate (..), StringUpdate (..), DoubleUpdate (..), BytesUpdate (..), Update (..),
  Value (..),
  create, createMany,
  findFirst, findMany, findUnique,
  updateMany, updateUnique,
  deleteMany, deleteUnique
) where

import Data.List (intercalate)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

data IntQuery = 
    IntEquals Int
  | IntNot Int
  | IntIn [Int]
  | IntNotIn [Int]
  | IntLt Int
  | IntLte Int
  | IntGt Int
  | IntGte Int

data StringQuery = 
    StringEquals String
  | StringNot String
  | StringIn [String]
  | StringNotIn [String]
  | StringLt String
  | StringLte String
  | StringGt String
  | StringGte String
  | StringContains String
  | StringStartsWith String
  | StringEndsWith String

data DoubleQuery = 
    DoubleEquals Double
  | DoubleNot Double
  | DoubleIn [Double]
  | DoubleNotIn [Double]
  | DoubleLt Double
  | DoubleLte Double 
  | DoubleGt Double
  | DoubleGte Double 

data BytesQuery = 
    BytesEquals ByteString
  | BytesNot ByteString
  | BytesIn [ByteString]
  | BytesNotIn [ByteString]

data Query =
    QInt String IntQuery
  | QString String StringQuery
  | QDouble String DoubleQuery
  | QBytes String BytesQuery
  | Or [Query]
  | Not Query

data IntUpdate = 
    IntSet Int
  | IntIncrement Int
  | IntDecrement Int
  | IntMultiply Int
  | IntDivide Int

data StringUpdate = StringSet String

data DoubleUpdate = 
    DoubleSet Double
  | DoubleIncrement Double
  | DoubleDecrement Double
  | DoubleMultiply Double
  | DoubleDivide Double

data BytesUpdate = BytesSet ByteString

data Update =
    UInt String IntUpdate
  | UString String StringUpdate
  | UDouble String DoubleUpdate
  | UBytes String BytesUpdate

data Value = 
    NullVal String
  | IntVal String Int
  | StringVal String String
  | DoubleVal String Double
  | BytesVal String ByteString

create :: String -> [Value] -> String
create table row =
  let (columns, valueLiterals) = unzip $ map renderValue row in
  let columnList = intercalate ", " columns in
  let valueList = intercalate ", " valueLiterals in
  "INSERT INTO " ++ table ++ " (" ++ columnList ++ ") VALUES (" ++ valueList ++ ")"

createMany :: String -> [[Value]] -> String
createMany table rows =
  let columns = extractColumns (head rows) in
  let columnList = intercalate ", " columns in
  let valueLists = map (\row -> "(" ++ intercalate ", " (map renderValueLiteral row) ++ ")") rows in
  let values = intercalate ", " valueLists in
  "INSERT INTO " ++ table ++ " (" ++ columnList ++ ") VALUES " ++ values

findMany :: String -> [Query] -> String
findMany table qs =
  let whereClause = if null qs then "" else " WHERE " ++ intercalate " AND " (map renderQuery qs) in
  "SELECT * FROM " ++ table ++ whereClause

findFirst :: String -> [Query] -> String
findFirst table qs = findMany table qs ++ " LIMIT 1"

findUnique :: String -> [Query] -> String
findUnique = findFirst

updateMany :: String -> [Query] -> [Update] -> String
updateMany table qs us =
  let whereClause = if null qs then "" else " WHERE " ++ intercalate " AND " (map renderQuery qs) in
  let setClause = intercalate ", " (map renderUpdate us) in
  "UPDATE " ++ table ++ " SET " ++ setClause ++ whereClause

{-updateFirst :: String -> [Query] -> [Update] -> String
updateFirst table qs us =
  let whereClause = if null qs then " LIMIT 1" else " WHERE " ++ intercalate " AND " (map renderQuery qs) ++ " LIMIT 1" in
  let setClause = intercalate ", " (map renderUpdate us) in
  "UPDATE " ++ table ++ " SET " ++ setClause ++ whereClause -}

updateUnique :: String -> [Query] -> [Update] -> String
updateUnique = updateMany

deleteMany :: String -> [Query] -> String
deleteMany table qs =
  let whereClause = if null qs then "" else " WHERE " ++ intercalate " AND " (map renderQuery qs) in
  "DELETE FROM " ++ table ++ whereClause

{-deleteFirst :: String -> [Query] -> String
deleteFirst table qs = deleteMany table qs ++ " LIMIT 1"-}

deleteUnique :: String -> [Query] -> String
deleteUnique = deleteMany

renderValue :: Value -> (String, String)
renderValue (NullVal field) = (field, "NULL")
renderValue (IntVal field val) = (field, show val)
renderValue (StringVal field val) = (field, "'" ++ val ++ "'")
renderValue (DoubleVal field val) = (field, show val)
renderValue (BytesVal field val) = (field, "X'" ++ BS.unpack val ++ "'")

renderValueLiteral :: Value -> String
renderValueLiteral val = snd $ renderValue val

extractColumns :: [Value] -> [String]
extractColumns = map (\v -> case v of
    NullVal field -> field
    IntVal field _ -> field
    StringVal field _ -> field
    DoubleVal field _ -> field
    BytesVal field _ -> field)

renderIntQuery :: String -> IntQuery -> String
renderIntQuery field (IntEquals n) = field ++ " = " ++ show n
renderIntQuery field (IntNot n) = field ++ " != " ++ show n
renderIntQuery field (IntIn ns) = field ++ " IN (" ++ intercalate ", " (map show ns) ++ ")"
renderIntQuery field (IntNotIn ns) = field ++ " NOT IN (" ++ intercalate ", " (map show ns) ++ ")"
renderIntQuery field (IntLt n) = field ++ " < " ++ show n
renderIntQuery field (IntLte n) = field ++ " <= " ++ show n
renderIntQuery field (IntGt n) = field ++ " > " ++ show n
renderIntQuery field (IntGte n) = field ++ " >= " ++ show n

renderStringQuery :: String -> StringQuery -> String
renderStringQuery field (StringEquals s) = field ++ " = '" ++ s ++ "'"
renderStringQuery field (StringNot s) = field ++ " != '" ++ s ++ "'"
renderStringQuery field (StringIn ss) = field ++ " IN (" ++ intercalate ", " (map (\s -> "'" ++ s ++ "'") ss) ++ ")"
renderStringQuery field (StringNotIn ss) = field ++ " NOT IN (" ++ intercalate ", " (map (\s -> "'" ++ s ++ "'") ss) ++ ")"
renderStringQuery field (StringLt s) = field ++ " < '" ++ s ++ "'"
renderStringQuery field (StringLte s) = field ++ " <= '" ++ s ++ "'"
renderStringQuery field (StringGt s) = field ++ " > '" ++ s ++ "'"
renderStringQuery field (StringGte s) = field ++ " >= '" ++ s ++ "'"
renderStringQuery field (StringContains s) = field ++ " LIKE '%" ++ s ++ "%'"
renderStringQuery field (StringStartsWith s) = field ++ " LIKE '" ++ s ++ "%'"
renderStringQuery field (StringEndsWith s) = field ++ " LIKE '%" ++ s ++ "'"

renderDoubleQuery :: String -> DoubleQuery -> String
renderDoubleQuery field (DoubleEquals d) = field ++ " = " ++ show d
renderDoubleQuery field (DoubleNot d) = field ++ " != " ++ show d
renderDoubleQuery field (DoubleIn ds) = field ++ " IN (" ++ intercalate ", " (map show ds) ++ ")"
renderDoubleQuery field (DoubleNotIn ds) = field ++ " NOT IN (" ++ intercalate ", " (map show ds) ++ ")"
renderDoubleQuery field (DoubleLt d) = field ++ " < " ++ show d
renderDoubleQuery field (DoubleLte d) = field ++ " <= " ++ show d
renderDoubleQuery field (DoubleGt d) = field ++ " > " ++ show d
renderDoubleQuery field (DoubleGte d) = field ++ " >= " ++ show d

renderBytesQuery :: String -> BytesQuery -> String
renderBytesQuery field (BytesEquals b) = field ++ " = X'" ++ BS.unpack b ++ "'"
renderBytesQuery field (BytesNot b) = field ++ " != X'" ++ BS.unpack b ++ "'"
renderBytesQuery field (BytesIn bs) = field ++ " IN (" ++ intercalate ", " (map (\b -> "X'" ++ BS.unpack b ++ "'") bs) ++ ")"
renderBytesQuery field (BytesNotIn bs) = field ++ " NOT IN (" ++ intercalate ", " (map (\b -> "X'" ++ BS.unpack b ++ "'") bs) ++ ")"

renderQuery :: Query -> String
renderQuery (QInt field iq) = renderIntQuery field iq
renderQuery (QString field sq) = renderStringQuery field sq
renderQuery (QDouble field dq) = renderDoubleQuery field dq
renderQuery (QBytes field bq) = renderBytesQuery field bq
renderQuery (Or qs) = "(" ++ intercalate " OR " (map renderQuery qs) ++ ")"
renderQuery (Not q) = "NOT (" ++ renderQuery q ++ ")"

renderIntUpdate :: String -> IntUpdate -> String
renderIntUpdate field (IntSet n) = field ++ " = " ++ show n
renderIntUpdate field (IntIncrement n) = field ++ " = " ++ field ++ " + " ++ show n
renderIntUpdate field (IntDecrement n) = field ++ " = " ++ field ++ " - " ++ show n
renderIntUpdate field (IntMultiply n) = field ++ " = " ++ field ++ " * " ++ show n
renderIntUpdate field (IntDivide n) = field ++ " = " ++ field ++ " / " ++ show n

renderStringUpdate :: String -> StringUpdate -> String
renderStringUpdate field (StringSet s) = field ++ " = '" ++ s ++ "'"

renderDoubleUpdate :: String -> DoubleUpdate -> String
renderDoubleUpdate field (DoubleSet d) = field ++ " = " ++ show d
renderDoubleUpdate field (DoubleIncrement d) = field ++ " = " ++ field ++ " + " ++ show d
renderDoubleUpdate field (DoubleDecrement d) = field ++ " = " ++ field ++ " - " ++ show d
renderDoubleUpdate field (DoubleMultiply d) = field ++ " = " ++ field ++ " * " ++ show d
renderDoubleUpdate field (DoubleDivide d) = field ++ " = " ++ field ++ " / " ++ show d

renderBytesUpdate :: String -> BytesUpdate -> String
renderBytesUpdate field (BytesSet b) = field ++ "= X'" ++ BS.unpack b ++ "'"

renderUpdate :: Update -> String
renderUpdate (UInt field ui) = renderIntUpdate field ui
renderUpdate (UString field su) = renderStringUpdate field su
renderUpdate (UDouble field du) = renderDoubleUpdate field du
renderUpdate (UBytes field bu) = renderBytesUpdate field bu
