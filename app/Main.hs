module Main where

import Lib
import Data.List.Split (splitOn)
import System.Environment (getEnv)
import System.Directory (listDirectory)

main :: IO ()
main = do
  putStrLn "hello world"

  -- | Get a String from getEnv "PATH"
  -- | return String
  path <- getEnv "PATH"

  -- | Split the path by ":"
  -- | return [[Char]]
  let paths = splitOn ":" path

  -- | Convert the [[Char]] to [FilePath]
  let fp_paths = map str_to_filepath paths

  -- | Get all binaries from the list of FilePath paths
  --let all_binaries = mapM listDirectory $ take 3 fp_paths
  all_binaries <- mapM listDirectory $ take 3 fp_paths
  mapM print all_binaries
  -- ^^ fails, saying that a path does not exist in $PATH: TODO

  -- | foreach path ([Char]): 

  putStrLn "ciao"

-- | Convert paths into [FilePath]
str_to_filepath :: [Char] -> FilePath
str_to_filepath [] = []
str_to_filepath (x:xs) = x : str_to_filepath xs

-- | lists of lists to list

