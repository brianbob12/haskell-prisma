module Main (main) where

import HaskellPrisma
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (schema:dir:_) -> (
      putStrLn ("Generating modules in " ++ dir ++ " from " ++ schema) >> 
      generateModule schema dir)
    _ -> putStrLn "Must prove two command-line arguments: the schema.prisma file \
                \to parse, and the directory to generate modules in"
