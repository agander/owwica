#!/usr/bin/env stack
-- stack --verbose --resolver lts-12.4 script

{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Main where

import qualified Data.Text as T
import Data.Time
import System.Environment
import System.Exit
import Data.Text.Internal (showText)
import System.IO (hPutStrLn, stderr)
import System.FilePath.Posix ((</>))
import Text.Printf

mv_args_to_text :: [String] -> [T.Text]
mv_args_to_text [] = []
mv_args_to_text (arg:args) = T.pack arg : mv_args_to_text args

put_args = do
  putStr "args: "
  getArgs >>= print . length
  getArgs >>= print

--get_l_data :: T.Text -> [T.Text]
get_str_l_data = ("a", ["b" , "c" , "d" ])

get_t_path = [T.pack "a"]
get_t_exes = [T.pack "b" , T.pack "c" , T.pack "d" ]
sep = T.pack "/"

get_tup_l_data :: (T.Text, [T.Text])
get_tup_l_data = (T.pack "a", [T.pack "b" , T.pack "c" , T.pack "d" ])

get_tup_t_data :: [(T.Text, [T.Text])]
get_tup_t_data = [( T.pack "a", [T.pack "b" , T.pack "c" , T.pack "d" ]), (T.pack "w", [T.pack "x" , T.pack "y" , T.pack "z" ])]

--join_path_to_exes :: (T.Text, [T.Text]) ->  [T.Text]
--join_path_to_exes p  [] = []
--join_path_to_exes (p, (exe:exes)) = (T.append p . T.append (T.pack "/")) exe : join_path_to_exes p exes

main :: IO ()
main = do
  args <- getArgs

  -- | exit if no args
  case args of
    [] ->        hPutStrLn stderr "str_to_test: no args" >> exitFailure
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
  mapM (printf "%s: ") [(T.append p . T.append sep) exe  | p <- get_t_path, exe <- get_t_exes]
  printf "\n"

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

