{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Control.Concurrent.Async (mapConcurrently)
import Data.ByteString.Builder (Builder, byteString, hPutBuilder)
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (sortBy)
import Data.Monoid ((<>), mconcat)
import Data.Ord (comparing)
import Data.Text.Encoding (encodeUtf8)
import System.IO (Handle, stdout, hSetBinaryMode, hSetBuffering, BufferMode(..))
import System.Posix.Directory.Foreign (DirType(..))
import System.Posix.Directory.Traversals (getDirectoryContents)
import System.Posix.FilePath (RawFilePath)

import Ignore (allowed, getIgnores)
import Types (Ignore(..),
              FileInfo,
              Name(..),
              Path(..),
              Scope(..),
              Target(..),
              Tree(..))
import Util ((</>))

--------------------------------------------------------------------------------
main :: IO ()
main = do
    hSetBinaryMode stdout True
    hSetBuffering stdout (BlockBuffering Nothing)
    render stdout =<< tree i d
  where
    i = S.fromList $ map (Literal Relative Directory) [".git", ".hg"] ++
                     map (Literal Relative All) [".gitignore", ".hgignore"]
    d = (DirType 4, (Path ".", Name "."))

--------------------------------------------------------------------------------
tree :: HashSet Ignore -> FileInfo -> IO Tree
tree i (DirType 4, fileInfo@(path@(Path p), _)) = do
    c <- map (info path) . drop 2 <$> getDirectoryContents p
    i' <- (`S.union` i) <$> getIgnores path c
    Folder fileInfo . filter nonEmpty <$>
        (mapConcurrently (tree i') . sort . filter (allowed i')) c
tree _ (DirType _, f) = return (File f)

--------------------------------------------------------------------------------
sort :: [FileInfo] -> [FileInfo]
sort = sortBy (comparing (snd . snd))

--------------------------------------------------------------------------------
info :: Path -> (DirType, RawFilePath) -> FileInfo
info p (d, n) = let name = Name n
                 in (d, (p </> name, name))

--------------------------------------------------------------------------------
nonEmpty :: Tree -> Bool
nonEmpty (Folder _ []) = False
nonEmpty _ = True

--------------------------------------------------------------------------------
render :: Handle -> Tree -> IO ()
render h = hPutBuilder h . mconcat . draw
  where
    draw :: Tree -> [Builder]
    draw (Folder (_, (Name n)) t) = build n : drawSubTrees t
    draw (File (_, (Name n))) = [build n]

    build :: RawFilePath -> Builder
    build b = byteString b <> byteString "\n"

    drawSubTrees :: [Tree] -> [Builder]
    drawSubTrees [] = []
    drawSubTrees [v] = shift s1 (byteString "    ") (draw v)
    drawSubTrees (v:vs)= shift s2 s3 (draw v) <> drawSubTrees vs

    shift :: Builder -> Builder -> [Builder] -> [Builder]
    shift first other = zipWith (<>) (first : repeat other)

--------------------------------------------------------------------------------
s1,s2,s3 :: Builder
s1 = byteString (encodeUtf8 "└── ")
s2 = byteString (encodeUtf8 "├── ")
s3 = byteString (encodeUtf8 "│   ")
