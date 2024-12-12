module HaskellPrisma
    ( generateModule
    ) where

import PrismaSchemaRaw
import PrismaParser
import PrismaGenerator

writeFiles :: String -> [(String, String)] -> IO ()
writeFiles _ [] = return ()
writeFiles dir ((file, contents):t) = do
  writeFile (dir ++ "/" ++ file) contents
  writeFiles dir t

-- Generate the module and write it to the given directory
-- generateModule (PathToSchemaFile, PathToOutputDir)
generateModule :: String -> String -> IO ()
generateModule schema dir = do
  schemaStr <- readFile schema
  let parseRes = parseSchema schemaStr
  case parseRes of
    Left e -> putStrLn e
    Right s -> let sourceCode = generate s in writeFiles dir sourceCode
