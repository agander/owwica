#!/usr/bin/env stack
-- stack --verbose --resolver lts-12.4 script

{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Main where

--import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Time
import System.Environment
import System.Exit
--import Data.Text.Internal (showText)
import System.IO (hPutStrLn, stderr)
import System.FilePath.Posix ((</>))
import Text.Printf
import Text.Regex.PCRE

import Lib

mv_args_to_bs :: [String] -> [B.ByteString]
mv_args_to_bs [] = []
mv_args_to_bs (arg:args) = C.pack arg : mv_args_to_bs args

put_args = do
  putStr "args: "
  getArgs >>= print . length
  getArgs >>= print

--get_l_data :: T.Text -> [T.Text]
get_str_l_data = ("a", ["b" , "c" , "d" ])

get_b_path = C.pack "a"

get_b_exes :: [C.ByteString]
get_b_exes = [C.pack "b" , C.pack "c"] -- , C.pack "d", C.pack "happy" ]
sep = C.pack "/"

get_tup_l_data :: (C.ByteString, [C.ByteString])
get_tup_l_data = (C.pack "a", [C.pack "b" , C.pack "c" , C.pack "d" ])

get_tup_b_data :: [(C.ByteString, [C.ByteString])]
get_tup_b_data = [( C.pack "a", [C.pack "b" , C.pack "c" , C.pack "d" ]), (C.pack "w", [C.pack "x" , C.pack "y" , C.pack "z" ])]

--join_path_to_exes :: (T.Text, [T.Text]) ->  [T.Text]
--join_path_to_exes p  [] = []
--join_path_to_exes (p, (exe:exes)) = (T.append p . T.append (C.pack "/")) exe : join_path_to_exes p exes

main :: IO ()
main = do
  args <- getArgs

  -- | exit if no args
  case args of
    --[]        -> hPutStrLn stderr "str_to_bytestr: no args" >> exitFailure
    []        -> hPutStrLn stderr "str_to_bytestr: no args"
    otherwise -> put_args

  -- | Move the agrs to a Text array
  let b_args = mv_args_to_bs args
  putStr ">>> COMMENT: b_args (getArgs converted to [ByteString]): *"
  mapM_ (print . show) b_args
  --let b_args_inters = C.intersperse '*' b_args
  --mapM_ (putStr . C.unpack) b_args_inters
  --putStr "\n"

  -- | Extract 
  all_bins <- mapM get_full_paths args
  let all_bins2 = concat all_bins
  putStr ">>> COMMENT: all_bins2 ([ByteString])"
  mapM_ (print . show) all_bins2

  -- | zip 2 lists
  --let path
  --mapM print [p : "/" : exe | p <- get_str_path, exe <- get_str_exes]
  mapM (printf "%s: " . C.unpack) [(C.append get_b_path . C.append sep) exe  | exe <- get_b_exes]
  printf "\n"
  let full_paths_semp =  [(C.append get_b_path . C.append sep) exe  | exe <- get_b_exes]
  putStr ">>> COMMENT: full_paths_semp are: *"
  mapM (C.putStr) full_paths_semp
  putStrLn "*"

  --let path_and_binsB = [(C.pack "/home/gander/.stack/snapshots/x86_64-linux-tinfo6/lts-12.4/8.4.3/bin",[C.pack "hspec-discover",C.pack "cpphs",C.pack "clientsession-generate",C.pack "ppsh",C.pack "alex",C.pack "vty-mode-demo",C.pack "hjsmin",C.pack "vty-demo",C.pack "happy"])]
  let pathB = C.pack "/home/gander/.stack/snapshots/x86_64-linux-tinfo6/lts-12.4/8.4.3/bin"
  let exesB = [C.pack "hspec-discover",C.pack "cpphs",C.pack "clientsession-generate",C.pack "ppsh",C.pack "alex",C.pack "vty-mode-demo",C.pack "hjsmin",C.pack "vty-demo",C.pack "happy"]
  let full_paths =  [(C.append pathB . C.append sep) exe  | exe <- exesB]
  putStr ">>> COMMENT: full_paths are: *"
  mapM C.putStr full_paths
  putStrLn "*"

  let pat3 = build_pat "vp"

  putStr ">>> COMMENT: 3rd test: supply a regex for a full path. pattern is: \""
  putStr pat3
  putStrLn "\""
  putStrLn ">>> COMMENT: and 'final_list' is:"
  --let final_list = filter (=~ pat3) full_paths
  let final_list = filter (=~ pat3) all_bins2
  --print $ filter (=~ pat3) tups_of_exe_plus_path'

  mapM_ print final_list

  myTime <- getZonedTime
  --putStr "myTime: "
  --print myTime 

  let utcTime = zonedTimeToUTC myTime
  --putStr "utcTime of myTime: "
  --print utcTime

  -- Add 5 minutes to the universal time
  let newUtcTime = addUTCTime (5*60) utcTime
  --putStr "at job time (current zoned time + 5: "
  --print newUtcTime

  -- Format the new time for at:
  -- -t time run the job at time, given in the format [[CC]YY]MMDDhhmm[.ss]
  --print $ formatTime defaultTimeLocale "%T, %F (%Z)" newUtcTime
  putStr $ formatTime defaultTimeLocale "%Y%m%d%H%M.%S" newUtcTime

-- | from <https://regexr.com/?2vpj8>
build_pat :: [Char] -> [Char]
build_pat pat = concat [ "([^/])*([\\w\\s-\\.]*" ++ pat ++ "[\\w\\s-\\.]*)$"]

-- | Convert paths into [FilePath]
str_to_filepath :: [Char] -> FilePath
str_to_filepath [] = []
str_to_filepath (x:xs) = x : str_to_filepath xs

{-
-- | Convert [FilePath] into paths
filepath_to_str :: FilePath -> [Char]
filepath_to_str [] = []
filepath_to_str (x:xs) = x : filepath_to_str xs
-}

