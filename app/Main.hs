module Main where

import Lib
import Data.List.Split (splitOn)
import System.Environment (getEnv)

main :: IO ()
main = do
  putStrLn "hello world"

  -- | Get a String from getEnv "PATH"
  path <- getEnv "PATH"

  putStrLn "ciao"

