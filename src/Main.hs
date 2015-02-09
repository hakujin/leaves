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
import System.Posix.FilePath ((</>), RawFilePath)
import Ignore (ignored, getIgnores)
import Types (Ignore(..), Scope(..), Target(..), Tree(..))

main :: IO ()
main = do
    hSetBinaryMode stdout True
    hSetBuffering stdout (BlockBuffering Nothing)
    render stdout =<< tree i d
    where
        i = S.fromList $ map (Literal Relative Directory) [".git", ".hg"]
        d = (DirType 4, (".", "."))

tree :: HashSet Ignore -> (DirType, (RawFilePath, RawFilePath)) -> IO Tree
tree i (DirType 4, f@(p, _)) = do
    c <- map (path p) . drop 2 <$> getDirectoryContents p
    i' <- (`S.union` i) <$> getIgnores p c
    Folder f <$> (mapConcurrently (tree i') . sort . filter (ignored i')) c
tree _ (DirType _, f) = return (File f)

sort :: [(DirType, (RawFilePath, RawFilePath))]
     -> [(DirType, (RawFilePath, RawFilePath))]
sort = sortBy (comparing (snd . snd))

path :: RawFilePath
     -> (DirType, RawFilePath)
     -> (DirType, (RawFilePath, RawFilePath))
path p (d, f) = (d, (p </> f, f))

render :: Handle -> Tree -> IO ()
render h = hPutBuilder h . mconcat . draw
    where
        draw :: Tree -> [Builder]
        draw (Folder (_, n) t) = (byteString n <> byteString "\n") :
            drawSubTrees t
        draw (File (_, n)) = [byteString n <> byteString "\n"]

        drawSubTrees :: [Tree] -> [Builder]
        drawSubTrees [v] = shift s1 (byteString "    ") (draw v)
        drawSubTrees (v:vs)= shift s2 s3 (draw v) <> drawSubTrees vs
        drawSubTrees _ = []

        shift :: Builder -> Builder -> [Builder] -> [Builder]
        shift first other = zipWith (<>) (first : repeat other)

s1,s2,s3 :: Builder
s1 = byteString (encodeUtf8 "└── ")
s2 = byteString (encodeUtf8 "├── ")
s3 = byteString (encodeUtf8 "│   ")
