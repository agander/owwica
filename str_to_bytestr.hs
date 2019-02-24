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

mv_args_to_text :: [String] -> [B.ByteString]
mv_args_to_text [] = []
mv_args_to_text (arg:args) = C.pack arg : mv_args_to_text args

put_args = do
  putStr "args: "
  getArgs >>= print . length
  getArgs >>= print

--get_l_data :: T.Text -> [T.Text]
get_str_l_data = ("a", ["b" , "c" , "d" ])

get_b_path = C.pack "a"
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
  let t_args = mv_args_to_text args
  --mapM_ (print . showText) t_args
  --let t_args_inters = T.intersperse '*' t_args
  --mapM_ (putStr . T.unpack) t_args_inters
  --putStr "\n"

  -- | join the fst to each of the snd's in a tuple of (Text, [Text])
  --let full_paths = map join_path_to_exes get_tup_l_data
  --mapM_ (print . showText) $ join_path_to_exes get_tup_l_data

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
  mapM (C.putStr) full_paths
  putStrLn "*"

  let pat3 = build_pat "ss"

  putStr ">>> COMMENT: 3rd test: supply a regex for a full path. pattern is: \""
  putStr pat3
  --putStrLn "\""
  putStrLn ">>> COMMENT: and the result is:"
  let final_list = filter (=~ pat3) full_paths
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