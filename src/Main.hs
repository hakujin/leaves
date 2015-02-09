{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.ST (runST)
import Data.ByteString.Builder (Builder, byteString, hPutBuilder)
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Monoid ((<>), mconcat)
import Data.Ord (comparing)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro (sortBy)
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
    c <- V.map (path p) . V.fromList . drop 2 <$> getDirectoryContents p
    i' <- (`S.union` i) <$> getIgnores p c
    Folder f <$> (mapConcurrently (tree i') . sort . V.filter (ignored i')) c
tree _ (DirType _, f) = return (File f)
{-# INLINABLE tree #-}

path :: RawFilePath
     -> (DirType, RawFilePath)
     -> (DirType, (RawFilePath, RawFilePath))
path p (d, f) = (d, (p </> f, f))
{-# INLINABLE path #-}

sort :: Vector (DirType, (RawFilePath, RawFilePath))
     -> Vector (DirType, (RawFilePath, RawFilePath))
sort v = runST $ do
    v' <- V.unsafeThaw v
    sortBy (comparing (snd . snd)) v'
    V.unsafeFreeze v'
{-# INLINABLE sort #-}

render :: Handle -> Tree -> IO ()
render h = hPutBuilder h . mconcat . draw
    where
        draw :: Tree -> [Builder]
        draw (Folder (_, n) t) = (byteString n <> byteString "\n") :
            drawSubTrees (V.toList t)
        draw (File (_, n)) = [byteString n <> byteString "\n"]
        {-# INLINABLE draw #-}

        drawSubTrees :: [Tree] -> [Builder]
        drawSubTrees [v] = shift s1 (byteString "    ") (draw v)
        drawSubTrees (v:vs)= shift s2 s3 (draw v) <> drawSubTrees vs
        drawSubTrees _ = []
        {-# INLINABLE drawSubTrees #-}

        shift :: Builder -> Builder -> [Builder] -> [Builder]
        shift first other = zipWith (<>) (first : repeat other)
        {-# INLINABLE shift #-}
{-# INLINABLE render #-}

s1,s2,s3 :: Builder
s1 = byteString (encodeUtf8 "└── ")
{-# INLINABLE s1 #-}
s2 = byteString (encodeUtf8 "├── ")
{-# INLINABLE s2 #-}
s3 = byteString (encodeUtf8 "│   ")
{-# INLINABLE s3 #-}
