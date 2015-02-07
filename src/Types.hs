module Types (
    File,
    Ignore(..),
    Tree(..)
) where

import Data.ByteString (ByteString)
import Data.Vector (Vector)
import System.Posix.Directory.Foreign (DirType(..))
import System.Posix.FilePath (RawFilePath)

type File = (DirType, RawFilePath)

data Ignore = Regex ByteString
            | Literal ByteString
            deriving (Show, Eq)

data Tree a = File a
            | Folder a (Vector (Tree a))
            deriving Show
