{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.ST (runST)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro (sortBy)
import System.Posix.Directory.Foreign (DirType(..))
import System.Posix.Directory.Traversals (getDirectoryContents)
import System.Posix.FilePath ((</>), takeFileName, RawFilePath)
import Ignore (ignore, ignored, readIgnores)
import Types (File, Ignore, Tree(..))

main :: IO ()
main = print =<< tree i f
  where
      i = V.map ignore [".", "..", ".git", ".gitignore", ".hg", ".hgignore"]
      f = (DirType 4, ".")

tree :: Vector Ignore -> File -> IO (Tree RawFilePath)
tree i (DirType 4, f) = do
    c <- V.fromList <$> getDirectoryContents f
    i' <- (<>) i <$> readIgnores f
    Folder (takeFileName f) <$>
        (mapConcurrently (tree i') . path f . sort . V.filter (ignored i')) c
tree _ (DirType _, f) = return (File $ takeFileName f)
{-# INLINABLE tree #-}

path :: RawFilePath -> Vector File -> Vector File
path f = V.map (second (f </>))
{-# INLINABLE path #-}

sort :: Vector File -> Vector File
sort v = runST $ do
    v' <- V.unsafeThaw v
    sortBy (comparing snd) v'
    V.unsafeFreeze v'
{-# INLINABLE sort #-}
