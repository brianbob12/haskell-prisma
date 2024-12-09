module Lib
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

generateModule :: String -> String -> IO ()
generateModule schema dir = do
  schemaStr <- readFile schema
  let parseRes = parseSchema schemaStr
  case parseRes of
    Left e -> putStrLn e
    Right s -> let sourceCode = generate s in writeFiles dir sourceCode
