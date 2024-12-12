module PrismaGenerator (generate) where

import Data.Char

import PrismaSchemaRaw

import FormatString

import GeneratorToolkit

-- take a schema, return list of filename, content pairs
generate :: Schema -> [(String, String)]
generate s =
  let client = genClient (models s) in
  let url = genUrl (databaseUrl s) in
  ("Client.hs", client) : ("ClientInternal.hs", clientInternal) : genModels (models s) url

genClient :: [Model] -> String
genClient = gen . map modelName where

  gen ms = unlines [
    clientStart,
    concat (map genExports ms),
    clientMid,
    concat (map genImports ms),
    concat (map genBindings ms)]

  clientStart = unlines [
    "module Client (",
    "  -- query constructors from ClientInternal",
    "  IntQuery (..), StringQuery (..), DoubleQuery (..), BytesQuery (..),",
    "  IntUpdate (..), StringUpdate (..), DoubleUpdate (..), BytesUpdate (..),",
    "  -- Result type from ClientInternal",
    "  Result (..),",
    "  "
    ]

  genExports m = unlinesFormatString [
    "  ",
    "  --Exports for ${name}",
    "  ${name}.${name} (..),",
    "  ${name}_Value (..), ${name}_Query (..), ${name}_Update (..),",
    "  ${nameLower}_create, ${nameLower}_createMany,",
    "  ${nameLower}_findFirst, ${nameLower}_findMany, ${nameLower}_findUnique,",
    "  ${nameLower}_updateMany, ${nameLower}_updateUnique,",
    "  ${nameLower}_deleteMany, ${nameLower}_deleteUnique"
    ] [("name", m), ("nameLower", makeLower m)]

  clientMid = ") where\n\nimport ClientInternal\n"

  genImports m = formatString "import qualified ${name}\n" [("name", m)]
  genBindings m = unlinesFormatString [
    "",
    "-- module bindings for ${name}",
    "type ${name}_Value = ${name}.Value",
    "type ${name}_Query = ${name}.Query",
    "type ${name}_Update = ${name}.Update",
    "",
    "${nameLower}_create = ${name}.create",
    "${nameLower}_createMany = ${name}.create",
    "${nameLower}_findFirst = ${name}.findFirst",
    "${nameLower}_findMany = ${name}.findMany",
    "${nameLower}_findUnique = ${name}.findUnique",
    "${nameLower}_updateMany = ${name}.updateMany",
    "${nameLower}_updateUnique = ${name}.updateUnique",
    "${nameLower}_deleteMany = ${name}.deleteMany",
    "${nameLower}_deleteUnique = ${name}.deleteUnique"
    ] [("name", m), ("nameLower", makeLower m)]
  makeLower [] = []
  makeLower (x:xs) = toLower x : xs


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
genModel modelName url fields = unlines [
    modelTop,
    genRecord,
    genValue,
    genQuery,
    genUpdate,
    "dbUrl :: IO String", url ++ "\n",
    "table :: String", "table = \"" ++ modelName ++ "\"\n",
    genResultType,
    genResultTuple,
    genSingleResult,
    genMapResults,
    modelFunctions,
    genToClientValues,
    genToClientQueries,
    genConvertValue,
    genToClientUpdate] where

  modelTop = unlinesFormatString [
    "module ${name} (",
    "  ${name} (..),",
    "  Value (..), Query (..), Update (..),",
    "  create, createMany,",
    "  findFirst, findMany, findUnique,",
    "  updateMany, updateUnique,",
    "  deleteMany, deleteUnique",
    ") where",
    "",
    "import qualified ClientInternal as CI",
    "import ClientInternal (Result (..))",
    "import qualified Database.SQLite.Simple as SQL",
    "import Data.String (fromString)",
    "import System.Environment (getEnv)",
    "import Control.Exception (try, SomeException)"
    ] [("name", modelName)]

  -- Generates the record type for the model
  genRecord = generateRecordData modelName
    (map (\field -> DataArg
      (formatString "get${modelName}${fieldName}"
        [("modelName", modelName),
         ("fieldName", capitalize $ fieldName field)])
      (typeMap $ fieldType field))
    fields)


  -- define the value type for the model
  genValue = generateSumData "Value"
    (map (\field -> DataArg
      (capitalize $ fieldName field)
      (typeMap $ fieldType field))
    fields)

  -- define the query type for the model

  queryDataArgs = DataArg "Or" "[Query]" : DataArg "Not" "Query" : map (\field -> DataArg
    (formatString
      "Q${fieldName}"
      [("fieldName", capitalize $ fieldName field)])
    (formatString
      "CI.${type}Query"
      [("type", typeMap $ fieldType field)])
    )
    fields

  genQuery = generateSumData "Query" queryDataArgs


  genUpdate = generateSumData "Update"
    (map (\field -> DataArg
      (formatString
        "U${fieldName}"
        [("fieldName", capitalize $ fieldName field)])
      (formatString
        "CI.${type}Update"
        [("type", typeMap $ fieldType field)])
      )
    fields)

  -- define an alias for the record type
  genResultType = formatString "type RecordType = ${modelName}" [("modelName", modelName)]

  -- define the find result type (from sqlite-simple query) for the model
  genResultTuple = formatString "type ResultTuple = (${members})" [("members", genResultMembers fields)] ++ "\n"

  genResultMembers [] = []
  genResultMembers [f] = let (_, _, typ) = fieldInfo f in typ
  genResultMembers (f:fs) = genResultMembers [f] ++ ", " ++ genResultMembers fs

  -- define the function which turns a result type into a record

  genSingleResult = unlinesFormatString [
      "singleResult :: [ResultTuple] -> Result ${modelName}",
      "singleResult (${singleResult} : _) = OK $ ${modelName} ${singleArgs}",
      "singleResult _ = SqlError \"No row found\"\n"
    ] [
      ("modelName", modelName),
      ("singleResult", singleResult), 
      ("singleArgs", singleArgs)
    ]

  singleResult =formatString
    "(${singleResultMembers})"
    [("singleResultMembers", singleResultMembers fields)]

  singleResultMembers [] = []
  singleResultMembers [f] = let (n, _, _) = fieldInfo f in n
  singleResultMembers (f:fs) = singleResultMembers [f] ++ ", " ++ singleResultMembers fs

  singleArgs = singleArgMembers fields
  singleArgMembers [] = []
  singleArgMembers (f:fs) = let (n, _, _) = fieldInfo f in
    n ++ " " ++ singleArgMembers fs

  -- define the function which turns a result list into a record list
  genMapResults = unlinesFormatString [
      "mapResults :: [ResultTuple] -> [${modelName}]",
      "mapResults = map (\\${singleResult} -> ${modelName} ${singleArgs})"
    ] [
      ("modelName", modelName),
      ("singleResult", singleResult),
      ("singleArgs", singleArgs)
    ]

  -- define the function which converts a value list into a general value list
  genToClientValues = unlines [
    "toClientValues :: [Value] -> Result [CI.Value]",
    "toClientValues values = ",
    "  if allThere values then OK $ map convertValue values",
    "    else SqlError \"Missing field(s)\"",
    "  where",
    unlines (convertValueV fields), 
    "    allThere = aux [" ++ genAllThere (filter fieldRequiredForCreation fields) ++ "]",
    "    aux [] _ = True; aux (f:fs) vs = any f vs && aux fs vs",
    unlines (genAllThereAuxes fields)
    ]

  convertValueV [] = []
  convertValueV (field:rest) = let (myFieldName, myFieldNameCapitalized, typ) = fieldInfo field in
    formatString
      "    convertValue (${fieldNameCapitalized} x) = CI.${type}Val \"${fieldName}\" x"
      [("fieldNameCapitalized", myFieldNameCapitalized), ("type", typ), ("fieldName", myFieldName)]
    : convertValueV rest

  genAllThere [] = []
  genAllThere [field] = let (_, fieldNameCapitalized, _) = fieldInfo field in
    formatString "is${fieldNameCapitalized}" [("fieldNameCapitalized", fieldNameCapitalized)]
  genAllThere (field:fields) = let (_, fieldNameCapitalized, _) = fieldInfo field in
    formatString
      "is${fieldNameCapitalized}, "
      [("fieldNameCapitalized", fieldNameCapitalized)]
    ++ genAllThere fields

  genAllThereAuxes [] = [""]
  genAllThereAuxes (field:rest) = 
    if fieldRequiredForCreation field then
      let (_, fieldNameCapitalized, _) = fieldInfo field in
      ("    is" ++ fieldNameCapitalized ++ " (" ++ fieldNameCapitalized ++ " _) = True; is" ++ fieldNameCapitalized ++ " _ = False") : genAllThereAuxes rest
    else
      genAllThereAuxes rest

  -- define the function which converts a query list into a general query list
  genToClientQueries = unlines [
    "toClientQueries :: [Query] -> [CI.Query]",
    "toClientQueries = map convertQuery",
    "  where", unlines (convertQuery fields),
    ""
    ]

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
  fieldInfo field = let myFieldName = fieldName field in
    (myFieldName, capitalize myFieldName, typeMap $ fieldType field)

  fieldHasAttribute :: Field -> Attribute -> Bool
  fieldHasAttribute field attribute = attribute `elem` attributes field

  attributeIsDefault :: Attribute -> Bool
  attributeIsDefault (DefaultAttribute _) = True
  attributeIsDefault _ = False

  fieldHasDefault :: Field -> Bool
  fieldHasDefault field = any attributeIsDefault $ attributes field

  typeIsOptional :: FieldType -> Bool
  typeIsOptional (OptionalField _) = True
  typeIsOptional _ = False

  fieldIsOptional :: Field -> Bool
  fieldIsOptional field = typeIsOptional $ fieldType field

  fieldRequiredForCreation :: Field -> Bool
  fieldRequiredForCreation field = not $ or [
    fieldIsOptional field,
    fieldHasDefault field,
    fieldHasAttribute field UpdatedAtAttribute,
    fieldHasAttribute field IgnoreAttribute
    ]

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
  "create :: [Value] -> IO (Result ())",
  "create values = do",
  "  url <- dbUrl",
  "  go (toClientValues values) url where",
  "    go (SqlError x) _ = return $ SqlError x",
  "    go (OK vals) url = do",
  "      let sqlQuery = CI.create table vals",
  "      result <- try $ do ",
  "        conn <- SQL.open url",
  "        SQL.execute_ conn (SQL.Query (fromString sqlQuery))",
  "        SQL.close conn",
  "      case result of",
  "        Left ex  -> return $ SqlError (show (ex :: SomeException))",
  "        Right () -> return $ OK ()",
  "",
  "createMany :: [[Value]] -> IO (Result ())",
  "createMany rows = do",
  "  url <- dbUrl",
  "  let rows' = toClientValues <$> rows",
  "  if any isError rows' then return $ SqlError \"Missing field(s)\"",
  "  else go (toRows rows') url where",
  "    go rs url = do",
  "      let sqlQuery = CI.createMany table rs",
  "      result <- try $ do",
  "        conn <- SQL.open url",
  "        SQL.execute_ conn (SQL.Query (fromString sqlQuery))",
  "        SQL.close conn",
  "      case result of",
  "        Left ex -> return $ SqlError (show (ex :: SomeException))",
  "        Right () -> return $ OK ()",
  "    isError (SqlError _) = True; isError _ = False",
  "    toRows (OK x : xs) = x : toRows xs",
  "    toRows _ = []",
  "",
  "findFirst :: [Query] -> IO (Result User)",
  "findFirst queries = do",
  "  url <- dbUrl",
  "  let sqlQuery = CI.findFirst table (toClientQueries queries)  ",
  "  result <- try $ do",
  "    conn <- SQL.open url",
  "    results <- SQL.query_ conn (SQL.Query (fromString sqlQuery)) :: IO [ResultTuple]",
  "    SQL.close conn",
  "    return $ singleResult results",
  "  case result of",
  "    Left ex -> return $ SqlError (show (ex :: SomeException))",
  "    Right x -> return x",
  "",
  "findMany :: [Query] -> IO (Result [User])",
  "findMany queries = do",
  "  url <- dbUrl",
  "  let sqlQuery = CI.findMany table (toClientQueries queries)",
  "  result <- try $ do",
  "    conn <- SQL.open url",
  "    results <- SQL.query_ conn (SQL.Query (fromString sqlQuery)) :: IO [ResultTuple]",
  "    SQL.close conn",
  "    return $ mapResults results",
  "  case result of",
  "    Left ex -> return $ SqlError (show (ex :: SomeException))",
  "    Right x -> return $ OK x",
  "",
  "findUnique :: [Value] -> IO (Result User)",
  "findUnique values = do",
  "  url <- dbUrl",
  "  let sqlQuery = CI.findFirst table (map convertValue values)",
  "  result <- try $ do",
  "    conn <- SQL.open url",
  "    results <- SQL.query_ conn (SQL.Query (fromString sqlQuery)) :: IO [ResultTuple]",
  "    SQL.close conn",
  "    return $ singleResult results",
  "  case result of",
  "    Left ex -> return $ SqlError (show (ex :: SomeException))",
  "    Right x -> return x",
  "",
  "updateMany :: [Query] -> [Update] -> IO (Result ())",
  "updateMany queries updates = do",
  "  url <- dbUrl",
  "  let sqlQuery = CI.updateMany table (toClientQueries queries) (map toClientUpdate updates)",
  "  result <- try $ do",
  "    conn <- SQL.open url",
  "    SQL.execute_ conn (SQL.Query (fromString sqlQuery))",
  "    SQL.close conn",
  "  case result of",
  "    Left ex -> return $ SqlError (show (ex :: SomeException))",
  "    Right () -> return $ OK ()",
  "",
  "updateUnique :: [Value] -> [Update] -> IO (Result ())",
  "updateUnique values updates = do",
  "  url <- dbUrl",
  "  let sqlQuery = CI.updateUnique table (map convertValue values) (map toClientUpdate updates)",
  "  result <- try $ do",
  "    conn <- SQL.open url",
  "    SQL.execute_ conn (SQL.Query (fromString sqlQuery))",
  "    SQL.close conn",
  "  case result of ",
  "    Left ex -> return $ SqlError (show (ex :: SomeException))",
  "    Right () -> return $ OK ()",
  "",
  "deleteMany :: [Query] -> IO (Result ())",
  "deleteMany queries = do",
  "  url <- dbUrl",
  "  let sqlQuery = CI.deleteMany table (toClientQueries queries)",
  "  result <- try $ do",
  "    conn <- SQL.open url",
  "    SQL.execute_ conn (SQL.Query (fromString sqlQuery))",
  "    SQL.close conn",
  "  case result of ",
  "    Left ex -> return $ SqlError (show (ex :: SomeException))",
  "    Right () -> return $ OK ()",
  "",
  "deleteUnique :: [Value] -> IO (Result ())",
  "deleteUnique values = do",
  "  url <- dbUrl",
  "  let sqlQuery = CI.deleteUnique table (map convertValue values)",
  "  result <- try $ do",
  "    conn <- SQL.open url",
  "    SQL.execute_ conn (SQL.Query (fromString sqlQuery))",
  "    SQL.close conn",
  "  case result of",
  "    Left ex -> return $ SqlError (show (ex :: SomeException))",
  "    Right () -> return $ OK ()"]

clientInternal = unlines [
  "module ClientInternal (",
  "  IntQuery (..), StringQuery (..), DoubleQuery (..), BytesQuery (..), Query (..),",
  "  IntUpdate (..), StringUpdate (..), DoubleUpdate (..), BytesUpdate (..), Update (..),",
  "  Value (..), Result (..),",
  "  getEnv,",
  "  create, createMany,",
  "  findFirst, findMany, findUnique,",
  "  updateMany, updateUnique,",
  "  deleteMany, deleteUnique",
  ") where",
  "",
  "import Data.List (intercalate)",
  "import Data.ByteString (ByteString)",
  "import qualified Data.ByteString.Char8 as BS",
  "import System.Environment (getEnv)",
  "",
  "data Result a = OK a | SqlError String deriving Show",
  "",
  "instance Functor Result where",
  "  fmap f (OK a) = OK (f a)",
  "  fmap _ (SqlError e) = SqlError e",
  "",
  "instance Applicative Result where",
  "  pure = OK",
  "  OK f <*> OK a = OK (f a)",
  "  SqlError e <*> _ = SqlError e",
  "  _ <*> SqlError e = SqlError e",
  "",
  "instance Monad Result where",
  "  return = pure",
  "  OK a >>= f = f a",
  "  SqlError e >>= _ = SqlError e",
  "",
  "data IntQuery = ",
  "    IntEquals Int",
  "  | IntNot Int",
  "  | IntIn [Int]",
  "  | IntNotIn [Int]",
  "  | IntLt Int",
  "  | IntLte Int",
  "  | IntGt Int",
  "  | IntGte Int deriving Show",
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
  "  | StringEndsWith String deriving Show",
  "",
  "data DoubleQuery = ",
  "    DoubleEquals Double",
  "  | DoubleNot Double",
  "  | DoubleIn [Double]",
  "  | DoubleNotIn [Double]",
  "  | DoubleLt Double",
  "  | DoubleLte Double ",
  "  | DoubleGt Double",
  "  | DoubleGte Double deriving Show",
  "",
  "data BytesQuery = ",
  "    BytesEquals ByteString",
  "  | BytesNot ByteString",
  "  | BytesIn [ByteString]",
  "  | BytesNotIn [ByteString] deriving Show",
  "",
  "data Query =",
  "    QInt String IntQuery",
  "  | QString String StringQuery",
  "  | QDouble String DoubleQuery",
  "  | QBytes String BytesQuery",
  "  | Or [Query]",
  "  | Not Query deriving Show",
  "",
  "data IntUpdate = ",
  "    IntSet Int",
  "  | IntIncrement Int",
  "  | IntDecrement Int",
  "  | IntMultiply Int",
  "  | IntDivide Int deriving Show",
  "",
  "data StringUpdate = StringSet String deriving Show",
  "",
  "data DoubleUpdate = ",
  "    DoubleSet Double",
  "  | DoubleIncrement Double",
  "  | DoubleDecrement Double",
  "  | DoubleMultiply Double",
  "  | DoubleDivide Double deriving Show",
  "",
  "data BytesUpdate = BytesSet ByteString deriving Show",
  "",
  "data Update =",
  "    UInt String IntUpdate",
  "  | UString String StringUpdate",
  "  | UDouble String DoubleUpdate",
  "  | UBytes String BytesUpdate deriving Show",
  "",
  "data Value = ",
  "    NullVal String",
  "  | IntVal String Int",
  "  | StringVal String String",
  "  | DoubleVal String Double",
  "  | BytesVal String ByteString deriving Show",
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
