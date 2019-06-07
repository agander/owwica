
module Lib
    ( get_full_paths
    , filepath_to_str
    , fp_to_bytestring
    , get_binaries
    , getAllPathContents
    , getAbsoluteDirContents
    , str_to_text
    , text_to_str
    , get_paths
    --, join_path_binary
    ) where

import System.Directory (listDirectory, getDirectoryContents)
import System.FilePath ((</>))
import qualified Data.Text as T
import Data.List.Split (splitOn)
import Data.Foldable (foldMap)
import qualified Data.ByteString.Char8 as C

--import Debug.Trace

{-# ANN module "HLint: ignore Use camelCase" #-}

get_full_paths :: FilePath -> IO [C.ByteString]
get_full_paths fp = do 
    all_binaries <- listDirectory fp
    --let fpB = C.pack $ filepath_to_str fp
    let fpS = Lib.filepath_to_str fp
    --return $ join_path_binary all_binaries fpB $ C.pack "/"
    return $ join_path_binary all_binaries fpS $ C.pack "/"

  -- | join_path_binary: 
--join_path_binary :: [FilePath] -> [Char] -> [Char] -> [C.ByteString]
join_path_binary :: [String] -> String -> C.ByteString -> [C.ByteString]
join_path_binary fp_binaries path sep =  [(C.append (C.pack path) . C.append sep) (C.pack exe)  | exe <- fp_binaries]

-- | Convert [FilePath] into paths
filepath_to_str :: FilePath -> String
filepath_to_str [] = []
filepath_to_str xs = xs

-- | fp_to_bytestring
fp_to_bytestring :: [String] -> [C.ByteString]
fp_to_bytestring [] = []
fp_to_bytestring (fp:fps) = C.pack fp : fp_to_bytestring fps

-- | get_binaries
get_binaries ::
  (Monad m, Traversable t) => t FilePath -> m (IO (t [FilePath]))
get_binaries fp = return $ mapM listDirectory fp

-- | No idea what this does
getAllPathContents :: Monad m => [FilePath] -> m [IO [FilePath]]
getAllPathContents paths = do
  --allFiles <- map getAbsoluteDirContents paths
  let allFiles = map getAbsoluteDirContents paths
  --return $ map T.pack allFiles
  return allFiles

{-
 - © Paolo Capriotti
 - <https://stackoverflow.com/questions/8572196/directory-contents-in-haskell/8572265#8572265>
 - I added the T.pack output
getAbsoluteDirContents :: String -> IO [FilePath]
-}
getAbsoluteDirContents :: FilePath -> IO [FilePath]
getAbsoluteDirContents dir = do
  contents <- getDirectoryContents dir
  --let fullFilePaths = map (dir </>) contents
  return $ map (dir </>) contents

strs_to_bytestrs :: [String] -> [C.ByteString]
strs_to_bytestrs [] = []
strs_to_bytestrs strs = map C.pack strs

str_to_bytestr :: String -> C.ByteString
str_to_bytestr = C.pack

str_to_text :: String -> T.Text
str_to_text = T.pack

text_to_str :: T.Text -> String
  --path <- T.pack $ getEnv "PATH"
text_to_str = T.unpack

-- From a String (${PATH}), return a List-of-Lists split on ':'
get_paths :: Eq a => [a] -> [a] -> [[a]]
get_paths path delimiter = splitOn delimiter path

toText :: [FilePath] -> T.Text
toText = foldMap T.pack

datumT :: T.Text
datumT = T.pack "cassava_eg.sh~\n\
  \Setup.hs\n\
  \.SimpleFFI.hs.un~\n\
  \stack.yaml\n.\n\
  \LTSB_sett_2017.csv\n\
  \dtmpl.cabal~\n\
  \LICENSE\ncassava_eg.sh\n\
  \dtmpl.cabal.un~\n\
  \git\n\
  \items.csv\n\
  \cassava_eg.txt\n\
  \gash.un~\n\
  \fsconfig.hs.un~\n\
  \SimpleFFI.hs~\n\
  \m4\n\
  \app\n\
  \fsconfig.sh~\n\
  \storia_it.html\n\
  \dtmpl.cabal\n\
  \SimpleFFI.hs\n\
  \stack.yaml~\n\
  \README.md\n\
  \src\n\
  \bytestr1.sh~\n\
  \test\n\
  \stack.yaml.un~\n\
  \fsconfig.sh\n\
  \fsconfig.hs~\n\
  \bytestr1.sh\n\
  \.gitignore\n\
  \.stack-work\n\
  \gash\n\
  \dsdt.dat\n\
  \.fsconfig.sh.un~\n"


