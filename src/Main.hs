{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative ((<$>))
import Control.Exception (handle, IOException)
import Data.List (sort)
import System.Directory (getDirectoryContents)
import Ignore (ignore, ignored, readIgnores, Ignore)

data Tree a = File a
            | Folder a [Tree a]
            deriving Show

main :: IO ()
main = print =<< tree is "."
  where
      is = map ignore [".", "..", ".git", ".gitignore", ".hg", ".hgignore"]

tree :: [Ignore] -> FilePath -> IO (Tree FilePath)
tree is f = handle (\(_ :: IOException) -> return $ File f) $ do
    cs <- getDirectoryContents f
    is' <- (++) is <$> readIgnores [".gitignore", ".hgignore"] f
    Folder f <$> (mapM (tree is') . sort . filter (ignored is')) cs
