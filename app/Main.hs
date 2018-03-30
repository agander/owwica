module Main where

import Lib
import Data.List.Split (splitOn)
import System.Environment (getEnv)
import System.Directory (listDirectory, doesDirectoryExist)
import System.IO.Unsafe
import Text.Regex.PCRE

main :: IO ()
main = do
  putStrLn ">>> COMMENT: program start"

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

  -- | Get all binaries from the list of FilePath paths
  all_binaries <- mapM listDirectory {-$ take 4-} good_fp_paths

  -- | and print a few
  putStrLn ">>> COMMENT: a [list] of all_binaries:"
  --print $ take 15 all_binaries

  -- | grab a few and concat them as thats the way they be at the end.
  let first_5 = concat {-. take 6 $-} all_binaries

  -- | convert all_binaries :: [FilePath] to [[Char]]
  let all_bins_strings = map filepath_to_str first_5

  -- | 

  -- | Regex.PCRE
  -- | set up a pattern to match on.
  -- | TODO: link this with getArgs
  let pat = "system"

  -- | filter first_5 for pat ("ghc")
  putStr ">>> COMMENT: pattern is: \""
  putStr pat
  putStrLn "\""

  putStrLn ">>> COMMENT: and the result is:"
  print $ filter (=~ pat) first_5
  -- filter (=~ pat2) all_bins_strings

  -- | foreach path ([Char]): 

  putStrLn ">>> COMMENT: fin"

-- | Convert paths into [FilePath]
str_to_filepath :: [Char] -> FilePath
str_to_filepath [] = []
str_to_filepath (x:xs) = x : str_to_filepath xs

-- | Convert [FilePath] into paths 
filepath_to_str :: FilePath -> [Char]
filepath_to_str [] = []
filepath_to_str (x:xs) = x : filepath_to_str xs

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

