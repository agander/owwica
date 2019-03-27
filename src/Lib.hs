module Lib
    ( get_full_paths
    , join_path_binary
    , filepath_to_str
    , fp_to_bytestring
    , get_binaries
    , getAllPathContents
    , getAbsoluteDirContents
    , str_to_text
    , text_to_str
    , get_paths
    , lose_io
    --, myzip
    --, myzipWith
    --, zeta
    --, blah'
    --, blah
    --, woot
    ) where

import System.Directory (listDirectory, getDirectoryContents)
import System.FilePath ((</>))
import qualified Data.Text as T
import Data.List.Split (splitOn)
--import qualified Data.List as L
import Data.Foldable (foldMap)
--import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Debug.Trace

get_full_paths :: FilePath -> IO [C.ByteString]
get_full_paths fp = do 
    all_binaries <- listDirectory fp
    --let fpB = C.pack $ filepath_to_str fp
    let fpS = Lib.filepath_to_str fp
    --return $ join_path_binary all_binaries fpB $ C.pack "/"
    return $ join_path_binary all_binaries fpS $ C.pack "/"

  -- | join_path_binary: 
--join_path_binary :: [FilePath] -> [Char] -> [Char] -> [C.ByteString]
join_path_binary fp_binaries path sep =  [(C.append (C.pack path) . C.append sep) (C.pack exe)  | exe <- fp_binaries]

-- | Convert [FilePath] into paths
filepath_to_str :: FilePath -> [Char]
filepath_to_str [] = []
filepath_to_str (x:xs) = x : filepath_to_str xs

-- | fp_to_bytestring
fp_to_bytestring :: [String] -> [C.ByteString]
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
--strs_to_bytestrs (str:strs) = (str_to_bytestr str) C.cons (strs_to_bytestrs strs)
strs_to_bytestrs (str:strs) = (C.pack str) : (strs_to_bytestrs strs)

str_to_bytestr :: String -> C.ByteString
str_to_bytestr = C.pack

str_to_text :: String -> T.Text
  --path <- T.pack $ getEnv "PATH"
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


-- | Small exercise from haskellbook - ignore
myzip :: [a] -> [b] -> [(a, b)]
myzip [] [] = []
myzip [_] [] = []
myzip [] [_] = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

-- | Small exercise from haskellbook - ignore
myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith f [] [] = []
myzipWith f [_] [] = []
myzipWith f [] [_] = []
myzipWith f (x:xs) (y:ys) = x `f` y : myzipWith f xs ys

--lose_io :: (IO a) -> a
lose_io :: a -> a
lose_io = id

blah :: IO String
blah = return "blah"

zeta :: IO Char
zeta = return 'z'

blah' = trace "outer trace" blah

woot :: IO String
woot = return (trace "inner trace" "woot")

