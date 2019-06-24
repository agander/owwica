
module Lib
    ( grep
    , get_full_paths
    , filepath_to_str
    , fp_to_bytestring
    , get_binaries
    , getAllPathContents
    , getAbsoluteDirContents
    , str_to_text
    , text_to_str
    , get_paths
    , doesDirectoryExist'
    , str_to_filepath
    , build_pat
    --, join_path_binary
    ) where

import System.Directory (listDirectory, getDirectoryContents)
import System.FilePath ((</>))
import qualified Data.Text as T
import Data.List.Split (splitOn)
import Data.Foldable (foldMap)
import qualified Data.ByteString.Char8 as C

import System.Directory (doesDirectoryExist )
import System.IO.Unsafe (unsafePerformIO)
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import System.IO

import Debug.Trace

grep :: C.ByteString -> FilePath -> IO ()
grep pattern file = withFile file ReadMode $ \h -> do
  is <- Streams.handleToInputStream h >>=
        Streams.lines                 >>=
        Streams.filter (C.isInfixOf pattern)
  os <- Streams.unlines Streams.stdout
  Streams.connect is os

{-# ANN module "HLint: ignore Use camelCase" #-}

-- | a version which extracts the value from the IO
-- Original: doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist' :: FilePath -> Bool
doesDirectoryExist' fp = unsafePerformIO $ doesDirectoryExist fp
{-# NOINLINE doesDirectoryExist' #-}

-- | Convert paths into [FilePath]
str_to_filepath :: String -> FilePath
str_to_filepath [] = []
str_to_filepath xs = xs

-- | ([^/])*([\w-\.]*ha[\w-\.]*)$
-- | from <https://regexr.com/?2vpj8>
build_pat :: String -> String
build_pat pat = "([^/])*([\\w\\s-\\.]*" ++ pat ++ "[\\w\\s-\\.]*)$"

{-
-- | Join 2 strings with FilePath.joinPath or (</>)
join_path :: String -> [String] -> [T.Text]
join_path _ [] = []
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
join_path' _ [] = []
join_path' path exes =
  map (\ exe -> T.unpack path ++ "/" ++ T.unpack exe) exes
--(T.unpack path ++ "/" ++ T.unpack exe) : join_path' path exes

-- | lists of lists to list

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

-}

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


