{-# LANGUAGE DeriveGeneric #-}

module Types (
    File,
    Ignore(..),
    Tree(..)
) where

import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import System.Posix.Directory.Foreign (DirType(..))
import System.Posix.FilePath (RawFilePath)

type File = (DirType, RawFilePath)

data Ignore = Regex ByteString
            | Literal ByteString
            deriving (Show, Eq, Generic)

instance Hashable Ignore

data Tree a = File a
            | Folder a (Vector (Tree a))
            deriving Show
