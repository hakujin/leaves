{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Types (
    Ignore(..),
    Name(..),
    Path(..),
    FileInfo,
    Scope(..),
    Target(..),
    Tree(..)
) where

import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import System.Posix.Directory.Foreign (DirType(..))
import System.Posix.FilePath (RawFilePath)

-- | Full file path.
newtype Path = Path RawFilePath deriving (Show, Eq, Ord)

-- | File name only.
newtype Name = Name RawFilePath deriving (Show, Eq, Ord)

-- | Structure that preserves all information about a File or Folder.
type FileInfo = (DirType, (Path, Name))

-- | Does the 'Ignore' match Files & Folders or just Folders?
data Target = All | Directory deriving (Show, Eq, Generic)

instance Hashable Target

-- | Does the 'Ignore' match relative or absolute paths?
data Scope = Relative | Absolute deriving (Show, Eq, Generic)

instance Hashable Scope

-- | A pattern based on literal or regex search, modified by matching rules.
data Ignore = Literal Scope Target ByteString
            | Regex Scope Target ByteString
            deriving (Show, Eq, Generic)

instance Hashable Ignore

-- | Models filesystem contents for the current project.
data Tree = File (Path, Name)
          | Folder (Path, Name) [Tree]
          deriving (Show, Eq, Ord)
