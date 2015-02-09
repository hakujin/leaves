{-# LANGUAGE DeriveGeneric #-}

module Types (
    Ignore(..),
    Scope(..),
    Target(..),
    Tree(..)
) where

import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import System.Posix.FilePath (RawFilePath)

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
data Tree = File (RawFilePath, RawFilePath)
          | Folder (RawFilePath, RawFilePath) (Vector Tree)
          deriving (Show, Eq, Ord)
