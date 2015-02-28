{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Control.Concurrent.Async (mapConcurrently)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Builder (Builder, byteString, hPutBuilder)
import Data.Foldable (fold)
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (sortBy)
import Data.Monoid ((<>), mconcat)
import Data.Ord (comparing)
import Data.Text.Encoding (encodeUtf8)
import System.Directory (getHomeDirectory)
import System.IO (Handle, stdout, hSetBinaryMode, hSetBuffering, BufferMode(..))
import System.Posix.Directory.Foreign (DirType(..))
import System.Posix.Directory.Traversals (getDirectoryContents)
import System.Posix.FilePath (RawFilePath)

import Ignore (allowed, getIgnores, readIgnore)
import Types (Ignore(..),
              FileInfo,
              Name(..),
              Path(..),
              Scope(..),
              Target(..),
              Tree(..))
import Util ((</>))

-- | Print the file structure of the current project, ignoring both empty
-- directories and patterns specified in version control .ignore files.
main :: IO ()
main = do
    h <- Path . pack <$> getHomeDirectory
    g <- fold <$> mapConcurrently (readIgnore h) (map Name d)
    hSetBinaryMode stdout True
    hSetBuffering stdout (BlockBuffering Nothing)
    render stdout =<< tree (l <> g) f
  where
    d = [".gitignore", ".hgignore"]
    l = S.fromList $ map (Literal Relative Directory) [".git", ".hg"] <>
                     map (Literal Relative All) d
    f = (DirType 4, (Path ".", Name "."))

-- | Recursively walk the filesystem, building a 'Tree' with the contents.
tree :: HashSet Ignore -> FileInfo -> IO Tree
tree i (DirType 4, fileInfo@(path@(Path p), _)) = do
    c <- map info . drop 2 <$> getDirectoryContents p -- drop ".", ".."
    i' <- (<> i) <$> getIgnores path c
    Folder fileInfo . filter nonEmpty <$>
        (mapConcurrently (tree i') . sort . filter (allowed i')) c
  where
    sort :: [FileInfo] -> [FileInfo]
    sort = sortBy (comparing (snd . snd))

    info :: (DirType, RawFilePath) -> FileInfo
    info (d, n) = let name = Name n
                  in (d, (path </> name, name))

    nonEmpty :: Tree -> Bool
    nonEmpty (Folder _ []) = False
    nonEmpty _ = True

tree _ (_, f) = return (File f)

-- | Print the 'Tree' to the 'Handle'.
render :: Handle -> Tree -> IO ()
render h = hPutBuilder h . mconcat . draw
  where
    build :: RawFilePath -> Builder
    build b = byteString b <> byteString "\n"

    draw :: Tree -> [Builder]
    draw (Folder (_, Name n) t) = build n : drawSubTrees t
    draw (File (_, Name n)) = [build n]

    drawSubTrees :: [Tree] -> [Builder]
    drawSubTrees [] = []
    drawSubTrees [v] = shift b1 (byteString "    ") (draw v)
    drawSubTrees (v:vs)= shift b2 b3 (draw v) <> drawSubTrees vs

    shift :: Builder -> Builder -> [Builder] -> [Builder]
    shift first other = zipWith (<>) (first : repeat other)

    b1,b2,b3 :: Builder
    b1 = byteString (encodeUtf8 "└── ")
    b2 = byteString (encodeUtf8 "├── ")
    b3 = byteString (encodeUtf8 "│   ")
