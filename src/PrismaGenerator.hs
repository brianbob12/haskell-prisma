module PrismaGenerator (generate) where

import Data.Char

import PrismaSchemaRaw

-- take a schema, return list of filename, content pairs
generate :: Schema -> [(String, String)]
generate s =
  let client = genClient (models s) in
  let url = genUrl (databaseUrl s) in
  ("Client.hs", client) : ("ClientInternal.hs", clientInternal) : genModels (models s) url

genClient :: [Model] -> String
genClient = gen . map modelName where
  gen ms = clientStart ++ concat (map genExports ms) ++ clientMid ++ concat (map genImports ms)
  clientStart = "module Client (\n\
                \  IntQuery (..), StringQuery (..), DoubleQuery (..), BytesQuery (..),\n\
                \  IntUpdate (..), StringUpdate (..), DoubleUpdate (..), BytesUpdate (..)\n"
  clientMid = ") where\n\nimport ClientInternal\n"
  genImports m = "import qualified " ++ m ++ "\n"
  genExports m = subst m "  , x.x (..),\n\
                         \  x.Value (..), x.Query (..), x.Update (..),\n\
                         \  x.create, x.createMany,\n\
                         \  x.findFirst, x.findMany, x.findUnique,\n\
                         \  x.updateMany, x.updateUnique,\n\
                         \  x.deleteMany, x.deleteUnique\n"

-- replaces every 'x' with x
subst :: String -> String -> String
subst x [] = []
subst x ('x':ss) = x ++ subst x ss
subst x (s:ss) = s : subst x ss

genUrl :: DatabaseURL -> String
genUrl (DirectURL url) = "dbUrl = return \"" ++ url ++ "\""
genUrl (EnvironmentVariable var) = "getEnv \"" ++ var ++ "\""

genModels :: [Model] -> String -> [(String, String)]
genModels ms url = map f ms where
  f m = 
    let n = modelName m in
    let fs = fields m in
    (n ++ ".hs", genModel n url fs)

genModel :: String -> String -> [Field] -> String
genModel name url fields = unlines [
    modelTop,
    genRecord,
    genValue,
    genQuery,
    genUpdate,
    "dbUrl :: IO String", url ++ "\n",
    "table :: String", "table = \"" ++ name ++ "\"\n",
    genResultType,
    genResultTuple,
    genSingleResult,
    genMapResults,
    modelFunctions,
    genToClientValues,
    genToClientQueries,
    genConvertValue,
    genToClientUpdate] where
  modelTop = subst name "module x (\n\
                        \  x (..),\n\
                        \  Value (..), Query (..), Update (..),\n\
                        \  create, createMany,\n\
                        \  findFirst, findMany, findUnique,\n\
                        \  updateMany, updateUnique,\n\
                        \  deleteMany, deleteUnique\n\
                        \) where\n\n\
                        \import qualified ClientInternal as CI\n\
                        \import qualified Database.SQLite.Simple as SQL\n\
                        \import Data.String (fromString)\n\
                        \import System.Environment (getEnv)\n"
  -- define the record type for the model
  genRecord = unlines [
    subst name "data x = x {",
    unlines (genRecordLines fields),
    "} deriving Show\n"]
  genRecordLines :: [Field] -> [String]
  genRecordLines [] = []
  genRecordLines [f] = let (_, name, typ) = fieldInfo f in
    ["  get" ++ name ++ " :: " ++ typ]
  genRecordLines (f:fs) = let (_, name, typ) = fieldInfo f in
    ("  get" ++ name ++ " :: " ++ typ ++ ",") : genRecordLines fs
  -- define the value type for the model
  genValue = unlines ["data Value =", unlines (genValueLines fields), ""]
  genValueLines :: [Field] -> [String]
  genValueLines [] = []
  genValueLines (f:fs) = ("    " ++ valueType f) : genValueRest fs
  genValueRest [] = []
  genValueRest (f:fs) = ("  | " ++ valueType f) : genValueRest fs
  valueType f = let (_, name, typ) = fieldInfo f in name ++ " " ++ typ
  -- define the query type for the model
  genQuery = unlines ["data Query =", "    Or [Query]", "  | Not Query",
    unlines (genQueryLines fields), ""]
  genQueryLines [] = []
  genQueryLines (f:fs) = let (_, name, typ) = fieldInfo f in
    ("  | Q" ++ name ++ " CI." ++ typ ++ "Query") : genQueryLines fs
  -- define the update type for the model
  genUpdate = unlines ["data Update =", unlines (genUpdateLines fields), ""]
  genUpdateLines [] = []
  genUpdateLines (f:fs) = ("    " ++ updateType f) : genUpdateRest fs
  genUpdateRest [] = []
  genUpdateRest (f:fs) = ("  | " ++ updateType f) : genUpdateRest fs
  updateType f = let (_, name, typ) = fieldInfo f in
    "U" ++ name ++ " CI." ++ typ ++ "Update"
  -- define an alias for the record type
  genResultType = "type RecordType = " ++ name
  -- define the find result type (from sqlite-simple query) for the model
  genResultTuple = "type ResultTuple = (" ++ genResultMembers fields ++ ")\n"
  genResultMembers [] = []
  genResultMembers [f] = let (_, _, typ) = fieldInfo f in typ
  genResultMembers (f:fs) = genResultMembers [f] ++ ", " ++ genResultMembers fs
  -- define the function which turns a result type into a record
  genSingleResult = unlines [
    "singleResult :: [ResultTuple] -> Maybe " ++ name,
    "singleResult (" ++ singleResult ++ " : _) = Just $ " ++ name ++ " " ++ singleArgs,
    "singleResult _ = Nothing\n"]
  singleResult = "(" ++ singleResultMembers fields ++ ")"
  singleResultMembers [] = []
  singleResultMembers [f] = let (n, _, _) = fieldInfo f in n
  singleResultMembers (f:fs) = singleResultMembers [f] ++ ", " ++ singleResultMembers fs
  singleArgs = singleArgMembers fields
  singleArgMembers [] = []
  singleArgMembers (f:fs) = let (n, _, _) = fieldInfo f in 
    n ++ " " ++ singleArgMembers fs
  -- define the function which turns a result list into a record list
  genMapResults = unlines [
    subst name "mapResults :: [ResultTuple] -> [x]",
    "mapResults = map (\\" ++ singleResult ++ " -> " ++ 
      name ++ " " ++ singleArgs ++ ")\n"]
  -- define the function which converts a value list into a general value list
  genToClientValues = unlines [
    "toClientValues :: [Value] -> [CI.Value]",
    "toClientValues values = map convertValue values",
    "  where", unlines (convertValueV fields), ""]
  convertValueV [] = []
  convertValueV (f:fs) = let (n, n', typ) = fieldInfo f in
    ("    convertValue (" ++ n' ++ " x) = CI." ++ typ ++ "Val \"" ++ n ++ "\" x") :
      convertValueV fs
  -- define the function which converts a query list into a general query list
  genToClientQueries = unlines [
    "toClientQueries :: [Query] -> [CI.Query]",
    "toClientQueries = map convertQuery",
    "  where", unlines (convertQuery fields), ""]
  convertQuery [] = []
  convertQuery (f:fs) = let (n, n', typ) = fieldInfo f in
    ("    convertQuery (Q" ++ n' ++ " q) = CI.Q" ++ typ ++ " \"" ++ n ++ "\" q") :
      convertQuery fs
  -- define the function which converts a value into a general query
  genConvertValue = unlines [
    "convertValue :: Value -> CI.Query", unlines (convertValue fields), ""]
  convertValue [] = []
  convertValue (f:fs) = let (n, n', typ) = fieldInfo f in
    ("convertValue (" ++ n' ++ " x) = CI.Q" ++ typ ++ " \"" ++ n ++ "\" (CI." ++
      typ ++ "Equals x)") : convertValue fs
  -- define the function which converts an update into a general update
  genToClientUpdate = unlines [
    "toClientUpdate :: Update -> CI.Update", unlines (toClientUpdate fields)]
  toClientUpdate [] = []
  toClientUpdate (f:fs) = let (n, n', typ) = fieldInfo f in
    ("toClientUpdate (U" ++ n' ++ " u) = CI.U" ++ typ ++ " \"" ++ n ++ "\" u") :
      toClientUpdate fs
  -- helpers
  fieldInfo :: Field -> (String, String, String)
  fieldInfo f = let n = fieldName f in (n, capitalize n, typeMap $ fieldType f)
  capitalize :: String -> String
  capitalize [] = []
  capitalize (c:cs) = toUpper c : cs
  typeMap :: FieldType -> String
  typeMap StringField = "String"
  typeMap IntField = "Int"
  typeMap BooleanField = "Int"
  typeMap DateTimeField = "Int"
  typeMap FloatField = "Double"
  typeMap DecimalField = "Double"
  typeMap BytesField = "ByteString"
  typeMap t = error ("unsupported type: " ++ show t)

modelFunctions = unlines [
  "create :: [Value] -> IO ()",
  "create values = do",
  "  url <- dbUrl",
  "  conn <- SQL.open url",
  "  let sqlQuery = CI.create table (toClientValues values)",
  "  SQL.execute_ conn (SQL.Query (fromString sqlQuery))",
  "  SQL.close conn",
  "",
  "createMany rows = do",
  "  url <- dbUrl",
  "  conn <- SQL.open url",
  "  let sqlQuery = CI.createMany table (map toClientValues rows)",
  "  SQL.execute_ conn (SQL.Query (fromString sqlQuery))",
  "  SQL.close conn",
  "",
  "findFirst :: [Query] -> IO (Maybe RecordType)",
  "findFirst queries = do",
  "  url <- dbUrl",
  "  conn <- SQL.open url",
  "  let sqlQuery = CI.findFirst table (toClientQueries queries)",
  "  results <- SQL.query_ conn (SQL.Query (fromString sqlQuery)) :: IO [ResultTuple]",
  "  SQL.close conn",
  "  return $ singleResult results",
  "",
  "findMany :: [Query] -> IO [RecordType]",
  "findMany queries = do",
  "  url <- dbUrl",
  "  conn <- SQL.open url",
  "  let sqlQuery = CI.findMany table (toClientQueries queries)",
  "  results <- SQL.query_ conn (SQL.Query (fromString sqlQuery)) :: IO [ResultTuple]",
  "  SQL.close conn",
  "  return $ mapResults results",
  "",
  "findUnique :: [Value] -> IO (Maybe RecordType)",
  "findUnique values = do",
  "  url <- dbUrl",
  "  conn <- SQL.open url",
  "  let sqlQuery = CI.findFirst table (map convertValue values)",
  "  results <- SQL.query_ conn (SQL.Query (fromString sqlQuery)) :: IO [ResultTuple]",
  "  SQL.close conn",
  "  return $ singleResult results",
  "",
  "updateMany :: [Query] -> [Update] -> IO ()",
  "updateMany queries updates = do",
  "  url <- dbUrl",
  "  conn <- SQL.open url",
  "  let sqlQuery = CI.updateMany table (toClientQueries queries) (map toClientUpdate updates)",
  "  SQL.execute_ conn (SQL.Query (fromString sqlQuery))",
  "  SQL.close conn",
  "",
  "updateUnique :: [Value] -> [Update] -> IO ()",
  "updateUnique values updates = do",
  "  url <- dbUrl",
  "  conn <- SQL.open url",
  "  let sqlQuery = CI.updateUnique table (map convertValue values) (map toClientUpdate updates)",
  "  SQL.execute_ conn (SQL.Query (fromString sqlQuery))",
  "  SQL.close conn",
  "",
  "deleteMany :: [Query] -> IO ()",
  "deleteMany queries = do",
  "  url <- dbUrl",
  "  conn <- SQL.open url",
  "  let sqlQuery = CI.deleteMany table (toClientQueries queries)",
  "  SQL.execute_ conn (SQL.Query (fromString sqlQuery))",
  "  SQL.close conn",
  "",
  "deleteUnique :: [Value] -> IO ()",
  "deleteUnique values = do",
  "  url <- dbUrl",
  "  conn <- SQL.open url",
  "  let sqlQuery = CI.deleteUnique table (map convertValue values)",
  "  SQL.execute_ conn (SQL.Query (fromString sqlQuery))",
  "  SQL.close conn"]

clientInternal = unlines [
  "module ClientInternal (",
  "  IntQuery (..), StringQuery (..), DoubleQuery (..), BytesQuery (..), Query (..),",
  "  IntUpdate (..), StringUpdate (..), DoubleUpdate (..), BytesUpdate (..), Update (..),",
  "  Value (..),",
  "  create, createMany,",
  "  findFirst, findMany, findUnique,",
  "  updateMany, updateUnique,",
  "  deleteMany, deleteUnique",
  ") where",
  "",
  "import Data.List (intercalate)",
  "import Data.ByteString (ByteString)",
  "import qualified Data.ByteString.Char8 as BS",
  "",
  "data IntQuery = ",
  "    IntEquals Int",
  "  | IntNot Int",
  "  | IntIn [Int]",
  "  | IntNotIn [Int]",
  "  | IntLt Int",
  "  | IntLte Int",
  "  | IntGt Int",
  "  | IntGte Int",
  "",
  "data StringQuery = ",
  "    StringEquals String",
  "  | StringNot String",
  "  | StringIn [String]",
  "  | StringNotIn [String]",
  "  | StringLt String",
  "  | StringLte String",
  "  | StringGt String",
  "  | StringGte String",
  "  | StringContains String",
  "  | StringStartsWith String",
  "  | StringEndsWith String",
  "",
  "data DoubleQuery = ",
  "    DoubleEquals Double",
  "  | DoubleNot Double",
  "  | DoubleIn [Double]",
  "  | DoubleNotIn [Double]",
  "  | DoubleLt Double",
  "  | DoubleLte Double ",
  "  | DoubleGt Double",
  "  | DoubleGte Double ",
  "",
  "data BytesQuery = ",
  "    BytesEquals ByteString",
  "  | BytesNot ByteString",
  "  | BytesIn [ByteString]",
  "  | BytesNotIn [ByteString]",
  "",
  "data Query =",
  "    QInt String IntQuery",
  "  | QString String StringQuery",
  "  | QDouble String DoubleQuery",
  "  | QBytes String BytesQuery",
  "  | Or [Query]",
  "  | Not Query",
  "",
  "data IntUpdate = ",
  "    IntSet Int",
  "  | IntIncrement Int",
  "  | IntDecrement Int",
  "  | IntMultiply Int",
  "  | IntDivide Int",
  "",
  "data StringUpdate = StringSet String",
  "",
  "data DoubleUpdate = ",
  "    DoubleSet Double",
  "  | DoubleIncrement Double",
  "  | DoubleDecrement Double",
  "  | DoubleMultiply Double",
  "  | DoubleDivide Double",
  "",
  "data BytesUpdate = BytesSet ByteString",
  "",
  "data Update =",
  "    UInt String IntUpdate",
  "  | UString String StringUpdate",
  "  | UDouble String DoubleUpdate",
  "  | UBytes String BytesUpdate",
  "",
  "data Value = ",
  "    NullVal String",
  "  | IntVal String Int",
  "  | StringVal String String",
  "  | DoubleVal String Double",
  "  | BytesVal String ByteString",
  "",
  "create :: String -> [Value] -> String",
  "create table row =",
  "  let (columns, valueLiterals) = unzip $ map renderValue row in",
  "  let columnList = intercalate \", \" columns in",
  "  let valueList = intercalate \", \" valueLiterals in",
  "  \"INSERT INTO \" ++ table ++ \" (\" ++ columnList ++ \") VALUES (\" ++ valueList ++ \")\"",
  "",
  "createMany :: String -> [[Value]] -> String",
  "createMany table rows =",
  "  let columns = extractColumns (head rows) in",
  "  let columnList = intercalate \", \" columns in",
  "  let valueLists = map (\\row -> \"(\" ++ intercalate \", \" (map renderValueLiteral row) ++ \")\") rows in",
  "  let values = intercalate \", \" valueLists in",
  "  \"INSERT INTO \" ++ table ++ \" (\" ++ columnList ++ \") VALUES \" ++ values",
  "",
  "findMany :: String -> [Query] -> String",
  "findMany table qs =",
  "  let whereClause = if null qs then \"\" else \" WHERE \" ++ intercalate \" AND \" (map renderQuery qs) in",
  "  \"SELECT * FROM \" ++ table ++ whereClause",
  "",
  "findFirst :: String -> [Query] -> String",
  "findFirst table qs = findMany table qs ++ \" LIMIT 1\"",
  "",
  "findUnique :: String -> [Query] -> String",
  "findUnique = findFirst",
  "",
  "updateMany :: String -> [Query] -> [Update] -> String",
  "updateMany table qs us =",
  "  let whereClause = if null qs then \"\" else \" WHERE \" ++ intercalate \" AND \" (map renderQuery qs) in",
  "  let setClause = intercalate \", \" (map renderUpdate us) in",
  "  \"UPDATE \" ++ table ++ \" SET \" ++ setClause ++ whereClause",
  "",
  "{-updateFirst :: String -> [Query] -> [Update] -> String",
  "updateFirst table qs us =",
  "  let whereClause = if null qs then \" LIMIT 1\" else \" WHERE \" ++ intercalate \" AND \" (map renderQuery qs) ++ \" LIMIT 1\" in",
  "  let setClause = intercalate \", \" (map renderUpdate us) in",
  "  \"UPDATE \" ++ table ++ \" SET \" ++ setClause ++ whereClause -}",
  "",
  "updateUnique :: String -> [Query] -> [Update] -> String",
  "updateUnique = updateMany",
  "",
  "deleteMany :: String -> [Query] -> String",
  "deleteMany table qs =",
  "  let whereClause = if null qs then \"\" else \" WHERE \" ++ intercalate \" AND \" (map renderQuery qs) in",
  "  \"DELETE FROM \" ++ table ++ whereClause",
  "",
  "{-deleteFirst :: String -> [Query] -> String",
  "deleteFirst table qs = deleteMany table qs ++ \" LIMIT 1\"-}",
  "",
  "deleteUnique :: String -> [Query] -> String",
  "deleteUnique = deleteMany",
  "",
  "renderValue :: Value -> (String, String)",
  "renderValue (NullVal field) = (field, \"NULL\")",
  "renderValue (IntVal field val) = (field, show val)",
  "renderValue (StringVal field val) = (field, \"'\" ++ val ++ \"'\")",
  "renderValue (DoubleVal field val) = (field, show val)",
  "renderValue (BytesVal field val) = (field, \"X'\" ++ BS.unpack val ++ \"'\")",
  "",
  "renderValueLiteral :: Value -> String",
  "renderValueLiteral val = snd $ renderValue val",
  "",
  "extractColumns :: [Value] -> [String]",
  "extractColumns = map (\\v -> case v of",
  "    NullVal field -> field",
  "    IntVal field _ -> field",
  "    StringVal field _ -> field",
  "    DoubleVal field _ -> field",
  "    BytesVal field _ -> field)",
  "",
  "renderIntQuery :: String -> IntQuery -> String",
  "renderIntQuery field (IntEquals n) = field ++ \" = \" ++ show n",
  "renderIntQuery field (IntNot n) = field ++ \" != \" ++ show n",
  "renderIntQuery field (IntIn ns) = field ++ \" IN (\" ++ intercalate \", \" (map show ns) ++ \")\"",
  "renderIntQuery field (IntNotIn ns) = field ++ \" NOT IN (\" ++ intercalate \", \" (map show ns) ++ \")\"",
  "renderIntQuery field (IntLt n) = field ++ \" < \" ++ show n",
  "renderIntQuery field (IntLte n) = field ++ \" <= \" ++ show n",
  "renderIntQuery field (IntGt n) = field ++ \" > \" ++ show n",
  "renderIntQuery field (IntGte n) = field ++ \" >= \" ++ show n",
  "",
  "renderStringQuery :: String -> StringQuery -> String",
  "renderStringQuery field (StringEquals s) = field ++ \" = '\" ++ s ++ \"'\"",
  "renderStringQuery field (StringNot s) = field ++ \" != '\" ++ s ++ \"'\"",
  "renderStringQuery field (StringIn ss) = field ++ \" IN (\" ++ intercalate \", \" (map (\\s -> \"'\" ++ s ++ \"'\") ss) ++ \")\"",
  "renderStringQuery field (StringNotIn ss) = field ++ \" NOT IN (\" ++ intercalate \", \" (map (\\s -> \"'\" ++ s ++ \"'\") ss) ++ \")\"",
  "renderStringQuery field (StringLt s) = field ++ \" < '\" ++ s ++ \"'\"",
  "renderStringQuery field (StringLte s) = field ++ \" <= '\" ++ s ++ \"'\"",
  "renderStringQuery field (StringGt s) = field ++ \" > '\" ++ s ++ \"'\"",
  "renderStringQuery field (StringGte s) = field ++ \" >= '\" ++ s ++ \"'\"",
  "renderStringQuery field (StringContains s) = field ++ \" LIKE '%\" ++ s ++ \"%'\"",
  "renderStringQuery field (StringStartsWith s) = field ++ \" LIKE '\" ++ s ++ \"%'\"",
  "renderStringQuery field (StringEndsWith s) = field ++ \" LIKE '%\" ++ s ++ \"'\"",
  "",
  "renderDoubleQuery :: String -> DoubleQuery -> String",
  "renderDoubleQuery field (DoubleEquals d) = field ++ \" = \" ++ show d",
  "renderDoubleQuery field (DoubleNot d) = field ++ \" != \" ++ show d",
  "renderDoubleQuery field (DoubleIn ds) = field ++ \" IN (\" ++ intercalate \", \" (map show ds) ++ \")\"",
  "renderDoubleQuery field (DoubleNotIn ds) = field ++ \" NOT IN (\" ++ intercalate \", \" (map show ds) ++ \")\"",
  "renderDoubleQuery field (DoubleLt d) = field ++ \" < \" ++ show d",
  "renderDoubleQuery field (DoubleLte d) = field ++ \" <= \" ++ show d",
  "renderDoubleQuery field (DoubleGt d) = field ++ \" > \" ++ show d",
  "renderDoubleQuery field (DoubleGte d) = field ++ \" >= \" ++ show d",
  "",
  "renderBytesQuery :: String -> BytesQuery -> String",
  "renderBytesQuery field (BytesEquals b) = field ++ \" = X'\" ++ BS.unpack b ++ \"'\"",
  "renderBytesQuery field (BytesNot b) = field ++ \" != X'\" ++ BS.unpack b ++ \"'\"",
  "renderBytesQuery field (BytesIn bs) = field ++ \" IN (\" ++ intercalate \", \" (map (\\b -> \"X'\" ++ BS.unpack b ++ \"'\") bs) ++ \")\"",
  "renderBytesQuery field (BytesNotIn bs) = field ++ \" NOT IN (\" ++ intercalate \", \" (map (\\b -> \"X'\" ++ BS.unpack b ++ \"'\") bs) ++ \")\"",
  "",
  "renderQuery :: Query -> String",
  "renderQuery (QInt field iq) = renderIntQuery field iq",
  "renderQuery (QString field sq) = renderStringQuery field sq",
  "renderQuery (QDouble field dq) = renderDoubleQuery field dq",
  "renderQuery (QBytes field bq) = renderBytesQuery field bq",
  "renderQuery (Or qs) = \"(\" ++ intercalate \" OR \" (map renderQuery qs) ++ \")\"",
  "renderQuery (Not q) = \"NOT (\" ++ renderQuery q ++ \")\"",
  "",
  "renderIntUpdate :: String -> IntUpdate -> String",
  "renderIntUpdate field (IntSet n) = field ++ \" = \" ++ show n",
  "renderIntUpdate field (IntIncrement n) = field ++ \" = \" ++ field ++ \" + \" ++ show n",
  "renderIntUpdate field (IntDecrement n) = field ++ \" = \" ++ field ++ \" - \" ++ show n",
  "renderIntUpdate field (IntMultiply n) = field ++ \" = \" ++ field ++ \" * \" ++ show n",
  "renderIntUpdate field (IntDivide n) = field ++ \" = \" ++ field ++ \" / \" ++ show n",
  "",
  "renderStringUpdate :: String -> StringUpdate -> String",
  "renderStringUpdate field (StringSet s) = field ++ \" = '\" ++ s ++ \"'\"",
  "",
  "renderDoubleUpdate :: String -> DoubleUpdate -> String",
  "renderDoubleUpdate field (DoubleSet d) = field ++ \" = \" ++ show d",
  "renderDoubleUpdate field (DoubleIncrement d) = field ++ \" = \" ++ field ++ \" + \" ++ show d",
  "renderDoubleUpdate field (DoubleDecrement d) = field ++ \" = \" ++ field ++ \" - \" ++ show d",
  "renderDoubleUpdate field (DoubleMultiply d) = field ++ \" = \" ++ field ++ \" * \" ++ show d",
  "renderDoubleUpdate field (DoubleDivide d) = field ++ \" = \" ++ field ++ \" / \" ++ show d",
  "",
  "renderBytesUpdate :: String -> BytesUpdate -> String",
  "renderBytesUpdate field (BytesSet b) = field ++ \"= X'\" ++ BS.unpack b ++ \"'\"",
  "",
  "renderUpdate :: Update -> String",
  "renderUpdate (UInt field ui) = renderIntUpdate field ui",
  "renderUpdate (UString field su) = renderStringUpdate field su",
  "renderUpdate (UDouble field du) = renderDoubleUpdate field du",
  "renderUpdate (UBytes field bu) = renderBytesUpdate field bu"]
