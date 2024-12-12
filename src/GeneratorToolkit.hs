module GeneratorToolkit where

import FormatString

data DataArg = DataArg {
  dataArgName :: String,
  dataArgType :: String
} deriving (Show, Eq)

generateRecordData :: String -> [ DataArg ] -> String
generateRecordData name args = 
  let 
    nameLine = "data ${name} = ${name} { "
    argsLines = generateArgLines args
    endLine = "} deriving Show"
  in
    unlinesFormatString
      (nameLine : argsLines ++ [endLine])
      [("name", name)]

generateArgLines :: [ DataArg ] -> [ String ]
generateArgLines [] = []
generateArgLines [DataArg name type'] = [generateArgLine (DataArg name type')]
generateArgLines (arg : args) = ((generateArgLine arg) ++ ",") : generateArgLines args

generateArgLine :: DataArg -> String
generateArgLine (DataArg name type') = formatString
  "  ${name} :: ${type'}"
  [("name", name), ("type'", type')]

generateSumData :: String -> [ DataArg ] -> String
generateSumData _ [] = "" -- Maybe throw error
generateSumData name (firstArg : restArgs) = 
  let 
    nameLine = "data " ++ name ++ " = "
    firstArgLine = "    " ++ dataArgName firstArg ++ " " ++ dataArgType firstArg
    restArgsLines = map (\(DataArg name type') -> "  | " ++ name ++ " " ++ type') restArgs
    derivingLines = "  deriving Show"
  in
    unlines (nameLine : firstArgLine : restArgsLines ++ [derivingLines])


