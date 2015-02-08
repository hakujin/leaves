{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

import Control.Applicative ((<$>))
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.ST (runST)
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Ord (comparing)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro (sortBy)
import System.Posix.Directory.Foreign (DirType(..))
import System.Posix.Directory.Traversals (getDirectoryContents)
import System.Posix.FilePath ((</>), takeFileName, RawFilePath)
import Ignore (ignore, ignored, getIgnores)
import Types (File, Ignore, Tree(..))

main :: IO ()
main = print =<< tree i d
  where
      i = S.map ignore [".git", ".gitignore", ".hg", ".hgignore"]
      d = (DirType 4, ".")

tree :: HashSet Ignore -> File -> IO (Tree RawFilePath)
tree i (DirType 4, d) = do
    c <- V.fromList . drop 2 <$> getDirectoryContents d
    i' <- (`S.union` i) <$> getIgnores d
    Folder (takeFileName d) <$>
        (mapConcurrently (tree i') . path d . sort . V.filter (ignored i')) c
tree _ (DirType _, f) = return (File f)
{-# INLINABLE tree #-}

path :: RawFilePath -> Vector File -> Vector File
path p = V.map step
  where
      step :: File -> File
      step (DirType 4, f) = (DirType 4, p </> f)
      step f = f
{-# INLINABLE path #-}

sort :: Vector File -> Vector File
sort v = runST $ do
    v' <- V.unsafeThaw v
    sortBy (comparing snd) v'
    V.unsafeFreeze v'
{-# INLINABLE sort #-}
