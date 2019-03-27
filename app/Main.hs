{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.List.Split (splitOn)
import System.Environment (getEnv)
import System.Directory (listDirectory, doesDirectoryExist )
import System.IO.Unsafe (unsafePerformIO)

import Text.Regex.PCRE
--import System.FilePath(joinPath, (</>))
import qualified Data.Text as T
import Text.Printf

import System.Console.CmdArgs
import Data.Version

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Lib

version :: Version
version = makeVersion [0,0,16]

insomma :: Version -> String-> String
insomma vsn str = str ++ " " ++ showVersion vsn

newtype Args = Args{ search_criteria :: String }
              deriving (Show, Data, Typeable)

owwica_cmd_hargs = Args{search_criteria = "ch"
               } &= 
               --{ debug &= def &= }
               summary (insomma version "Version:") &= 
               program "owwica" &=
               verbosityArgs [ignore] [name "silent", explicit] &=
               details ["More details on github.com/agander/owwica"] &=
               help "owwica:Usage: [-?/--help] [-V/--version] [--numeric-version] [-v|--verbose] "

main :: IO ()
main = do
  hargs <- cmdArgs owwica_cmd_hargs
  --putStrLn ">>> COMMENT: hargs"
  --print =<< cmdArgs owwica_cmd_hargs
  --printf "%s: %s\n" "search_criteria" (search_criteria hargs)

  --putStrLn ">>> COMMENT: program start"

  -- | Get a String from getEnv "PATH"
  -- | return String
  path <- getEnv "PATH"

  -- | Split the path by ":"
  -- | return [[Char]]
  let paths = splitOn ":" path
  let paths_2 = take 3 paths
  --putStrLn ">>> COMMENT: a [list] of paths:"
  --print paths --_2

  -- | Convert the [[Char]] to [FilePath]
  let fp_paths = map str_to_filepath paths

  -- | Check that the fp_paths are good
  -- | return [FilePath]
  let good_fp_paths = filter doesDirectoryExist' fp_paths

  -- | foreach good_fp_path: do listDirectory and attach the path
  -- | giving a full_path
  --let full_paths = build_full_paths good_fp_paths -- >>= fp_to_bytestring

  -- | Get all binaries from the list of FilePath paths
  all_binaries <- mapM listDirectory $ take 5 good_fp_paths

  -- | (attempt to) zip a few paths and their binaries
  let paths_and_bins = zip paths_2 all_binaries

  -- | and print a few
  --putStrLn ">>> COMMENT: the concat'd [list] of all_binaries:"
  ----print $ concat $ take 15 all_binaries
  --print $ concat all_binaries

  --putStrLn ">>> COMMENT: The zip\'ed (list of tuples) of each path and its binaries:"
  --print paths_and_bins
  let path_and_bins = [("/home/gander/.stack/snapshots/x86_64-linux-tinfo6/lts-12.4/8.4.3/bin",["hspec-discover","cpphs","clientsession-generate","ppsh","alex","vty-mode-demo","hjsmin","vty-demo","happy"])]
  let path_and_binsT = [(T.pack "/home/gander/.stack/snapshots/x86_64-linux-tinfo6/lts-12.4/8.4.3/bin",[T.pack "hspec-discover",T.pack "cpphs",T.pack "clientsession-generate",T.pack "ppsh",T.pack "alex",T.pack "vty-mode-demo",T.pack "hjsmin",T.pack "vty-demo",T.pack "happy"])]
  let path_and_binsB = [(C.pack "/home/gander/.stack/snapshots/x86_64-linux-tinfo6/lts-12.4/8.4.3/bin",[C.pack "hspec-discover",C.pack "cpphs",C.pack "clientsession-generate",C.pack "ppsh",C.pack "alex",C.pack "vty-mode-demo",C.pack "hjsmin",C.pack "vty-demo",C.pack "happy"])]
  --putStrLn ">>> COMMENT: map the 'fst' of 'path_and_bins':"
  --print $ map fst path_and_bins
  ----let fst_path = fst path_and_bins
  --putStrLn ">>> COMMENT: map the 'snd' of 'path_and_bins' and concat:"
  --print $ concatMap snd path_and_bins
  ----print map (joinPath' fst_path ) (snd path_and_bins)

  putStrLn "!!! TEST: A join of (fst path_and_bins !! 0) and (snd path_and_bins !! 0):"
  let tups_of_exe_plus_path = uncurry join_path (head path_and_bins)
  mapM_ (print . T.unpack) tups_of_exe_plus_path

  -- | grab a few and concat them as thats the way they will be in the end.
  let first_5 = concat {-. take 6 $-} all_binaries

  -- | convert all_binaries :: [FilePath] to [[Char]]
  let all_bins_strings = map filepath_to_str first_5

  -- | convert all_binaries :: [FilePath] to [[T.Text]]
  --let all_bins_text = T.map filepath_to_text first_5
  -- | ^^^ poss redundant ^^^

  -- |

  -- | Regex.PCRE
  -- | set up a pattern to match on.
  --let pat = (search_criteria hargs) --"pp"
  let pat = build_pat (search_criteria hargs)

  -- | filter first_5 for pat ("ghc")
  --putStr ">>> COMMENT: pattern is: \""
  --putStr pat
  --putStrLn "\""
  ----putStr ">>> COMMENT: data is: \""
  ----mapM_ print first_5

  --putStrLn ">>> COMMENT: and the result is:"
  --print $ filter (=~ pat) first_5

  -- filter (=~ pat2) all_bins_strings

  -- | test to match 'us' in a list of full path exes
  --let pat2_test_2 = ["/usr/bin/usb-devices", "/usr/bin/usbhid-dump", "/usr/sbin/usb_modeswitch", "/usr/sbin/usb_modeswitch_dispatcher", "/usr/sbin/usbmuxd", "/usr/sbin/useradd"]
  --let pat2_test_3 = ["/usr/bin/devices", "/uzr/bin/usbhid-dump", "/usr/bin/dog", "/usr/bin/gdb" ]
  --let pat2 = "/.*us.*$"
  --printf "%s: [%s] in: %s" ">>> COMMENT: test for " pat2 "\"/usr/bin/devices\", \"/uzr/bin/usbhid-dump\""
  --print $ filter (=~ pat2) pat2_test_3

  -- | test to match whatever is a main arg to the list of full path exes
  let pat3 = build_pat (search_criteria hargs)
  -- | swap the comments on these next two for debugging
  let tups_of_exe_plus_path' = uncurry join_path' (head path_and_binsT)
  --let tups_of_exe_plus_path' = uncurry join_path' $ take 2 path_and_bins
  --let tups_of_exe_plus_path' = uncurry join_path path_and_bins

  putStr ">>> COMMENT: 3rd test: supply a regex for a full path. pattern (pat3) is: \""
  putStr pat3
  putStrLn "\""
  putStrLn ">>> COMMENT: 'final_list' contains:"
  let final_list = filter (=~ pat3) tups_of_exe_plus_path'
  --print $ filter (=~ pat3) tups_of_exe_plus_path'
  mapM_ print final_list

  --putStrLn ">>> COMMENT: fin"
{-
-- | build_full_paths $ take 5 good_fp_paths
--build_full_paths :: [FilePath] -> [C.ByteString]
--build_full_paths (fp:fps) = (get_binaries fp >>= join_path_binary (str_to_filepath fp)) : build_full_paths fps
build_full_paths (fp:fps) = [full_paths : build_full_paths fps]
  where
    full_paths = do
      contents <- getAbsoluteDirContents fp
      return (contents)
-}

-- | Join 2 strings with FilePath.joinPath or (</>)
join_path :: [Char] -> [[Char]] -> [T.Text]
join_path path _ = []
join_path path (exe:exes) = T.pack (path ++ "/" ++ exe) : join_path path exes

sep :: C.ByteString
sep = C.pack "/"

slash :: T.Text
slash = T.pack "/"

-- let path_and_bins = [("/home/gander/.stack/snapshots/x86_64-linux-tinfo6/lts-11.1/8.2.2/bin",["hspec-discover","cpphs","clientsession-generate","ppsh","alex","vty-mode-demo","hjsmin","vty-demo","happy"])]
merge_exe_and_path :: [(T.Text, [T.Text])] -> [(T.Text, T.Text)]
merge_exe_and_path = undefined
--merge_exe_and_path (path, exes) = [ (a,b) | a <- path, b <- exes ]
--(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),

-- | Expand out a tuple of: ("pat/h", ["exe1", "exe2", "exe3", ..]) into:
--
join_path' :: T.Text -> [T.Text] -> [String]
join_path' path [] = []
join_path' path (exe:exes) =  (T.unpack path ++ "/" ++ T.unpack exe) : join_path' path exes
--join_path' = undefined

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
    return $! extcase () of {_
        | i == 0    -> (empty, PS x s l)
        | i == l    -> (PS x s l, empty)
        | otherwise -> (PS x s i, PS x (s+i) (l-i))
-}


-- | Convert [FilePath] to ByteString
filepath_to_bytes :: FilePath -> [C.ByteString]
filepath_to_bytes [] = []
filepath_to_bytes xs = map C.singleton xs

-- | Convert [FilePath] to Text
filepath_to_text :: FilePath -> [T.Text]
filepath_to_text [] = []
filepath_to_text xs = map T.singleton xs


-- | ([^/])*([\w-\.]*ha[\w-\.]*)$
-- | from <https://regexr.com/?2vpj8>
build_pat :: [Char] -> [Char]
build_pat pat = concat [ "([^/])*([\\w\\s-\\.]*" ++ pat ++ "[\\w\\s-\\.]*)$"]

