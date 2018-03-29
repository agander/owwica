module Main where

import Lib
import Data.List.Split (splitOn)
import System.Environment (getEnv)
import System.Directory (listDirectory, doesDirectoryExist)
import System.IO.Unsafe

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

  -- | Check that the fp_paths are good
  -- | return [FilePath]
  let good_fp_paths = filter doesDirectoryExist' fp_paths
  --isDoesNotExistError
  -- | Get all binaries from the list of FilePath paths
  --let all_binaries = mapM listDirectory $ take 3 good_fp_paths
  all_binaries <- mapM listDirectory $ take 3 good_fp_paths
  print $ take 5 all_binaries
  -- ^^ fails, saying that a path does not exist in $PATH: TODO

  -- | foreach path ([Char]): 

  putStrLn "ciao"

-- | Convert paths into [FilePath]
str_to_filepath :: [Char] -> FilePath
str_to_filepath [] = []
str_to_filepath (x:xs) = x : str_to_filepath xs

-- | lists of lists to list

-- | a version which extracts the value from the IO
-- Original: doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist' :: FilePath -> Bool
doesDirectoryExist' fp = unsafePerformIO $ doesDirectoryExist fp
{-# NOINLINE doesDirectoryExist' #-}

{- | 'breakSpace' returns the pair of ByteStrings when the argument is
-- broken at the first whitespace byte. I.e.
--
-- > break isSpace == breakSpace
--
breakSpace :: ByteString -> (ByteString,ByteString)
breakSpace (PS x s l) = accursedUnutterablePerformIO $ withForeignPtr x $ \p -> do
    i <- firstspace (p `plusPtr` s) 0 l
    return $! case () of {_
        | i == 0    -> (empty, PS x s l)
        | i == l    -> (PS x s l, empty)
        | otherwise -> (PS x s i, PS x (s+i) (l-i))
-}

