module Main where

import Lib
import Data.List.Split (splitOn)
import System.Environment (getEnv)
import System.Directory (listDirectory, doesDirectoryExist )
import System.IO.Unsafe
import Text.Regex.PCRE
import System.FilePath(joinPath, (</>))
import qualified Data.Text as T

main :: IO ()
main = do
  putStrLn ">>> COMMENT: program start"

  -- | Get a String from getEnv "PATH"
  -- | return String
  path <- getEnv "PATH"

  -- | Split the path by ":"
  -- | return [[Char]]
  let paths = splitOn ":" path
  let paths_2 = take 2 paths
  putStrLn ">>> COMMENT: a [list] of paths:"
  print paths_2

  -- | Convert the [[Char]] to [FilePath]
  let fp_paths = map str_to_filepath paths

  -- | Check that the fp_paths are good
  -- | return [FilePath]
  let good_fp_paths = filter doesDirectoryExist' fp_paths

  -- | Get all binaries from the list of FilePath paths
  all_binaries <- mapM listDirectory $ take 2 good_fp_paths

  -- | (attempt to) zip a few paths and their binaries
  let paths_and_bins = zip paths_2 all_binaries

  -- | and print a few
  putStrLn ">>> COMMENT: the concat'd [list] of all_binaries:"
  print $ concat $ take 15 all_binaries

  putStrLn ">>> COMMENT: The zip\'ed (list of tuples) of each path and its binaries:"
  print paths_and_bins
  let path_and_bins = [("/home/gander/.stack/snapshots/x86_64-linux-tinfo6/lts-11.1/8.2.2/bin",["hspec-discover","cpphs","clientsession-generate","ppsh","alex","vty-mode-demo","hjsmin","vty-demo","happy"])]
  putStrLn ">>> COMMENT: map the 'fst' of 'path_and_bins':"
  print $ map fst path_and_bins
  --let fst_path = fst path_and_bins
  putStrLn ">>> COMMENT: map the 'snd' of 'path_and_bins' and concat:"
  print $ concat $ map snd path_and_bins
  --print map (joinPath' fst_path ) (snd path_and_bins)

  -- | grab a few and concat them as thats the way they will be in the end.
  let first_5 = concat {-. take 6 $-} all_binaries

  -- | convert all_binaries :: [FilePath] to [[Char]]
  let all_bins_strings = map filepath_to_str first_5

  -- | convert all_binaries :: [FilePath] to [[T.Text]]
  --let all_bins_text = T.map filepath_to_text first_5

  -- | 

  -- | Regex.PCRE
  -- | set up a pattern to match on.
  -- | TODO: link this with getArgs
  let pat = "pp"

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
    return $! extcase () of {_
        | i == 0    -> (empty, PS x s l)
        | i == l    -> (PS x s l, empty)
        | otherwise -> (PS x s i, PS x (s+i) (l-i))
-}


-- | Join 2 strings with FilePath.joinPath or (</>)
--join_path :: String -> [T.Text] -> [T.Text]
join_path path [] = []
join_path path (exe:exes) = T.pack (path ++ "/" ++ exe) : join_path path exes


slash :: T.Text
slash = T.pack "/"

-- | Convert [FilePath] to Text
--filepath_to_text :: FilePath -> [T.Text]
filepath_to_text [] = []
filepath_to_text (x:xs) = T.pack x : filepath_to_text xs



