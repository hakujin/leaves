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

--------------------------------------------------------------------------------
newtype Path = Path RawFilePath deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
newtype Name = Name RawFilePath deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
type FileInfo = (DirType, (Path, Name))

--------------------------------------------------------------------------------
data Target = All | Directory deriving (Show, Eq, Generic)

instance Hashable Target

--------------------------------------------------------------------------------
data Scope = Relative | Absolute deriving (Show, Eq, Generic)

instance Hashable Scope

--------------------------------------------------------------------------------
data Ignore = Literal Scope Target ByteString
            | Regex Scope Target ByteString
            deriving (Show, Eq, Generic)

instance Hashable Ignore

--------------------------------------------------------------------------------
data Tree = File (Path, Name)
          | Folder (Path, Name) [Tree]
          deriving (Show, Eq, Ord)
