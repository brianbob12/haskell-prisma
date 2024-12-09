module GeneratorToolkit where

data DataArg = DataArg {
  dataArgName :: String,
  dataArgType :: String
} deriving (Show, Eq)

generateRecordDataArg :: String -> [ DataArg ] -> String
generateRecordDataArg name args = 
  let 
    nameLine = "data " ++ name ++ " = " ++ name ++ " { "
    argsLines = map (\(DataArg name type') -> "  " ++ name ++ " :: " ++ type') args
    endLine = "  }"
  in
    unlines (nameLine : argsLines ++ [endLine])

generateSumDataArg :: String -> [ DataArg ] -> String
generateSumDataArg _ [] = "" -- Maybe throw error
generateSumDataArg name (firstArg : restArgs) = 
  let 
    nameLine = "data " ++ name ++ " = "
    firstArgLine = "  " ++ dataArgName firstArg ++ " " ++ dataArgType firstArg
    restArgsLines = map (\(DataArg name type') -> "  " ++ name ++ " " ++ type') restArgs
  in
    unlines (nameLine : firstArgLine : restArgsLines)


