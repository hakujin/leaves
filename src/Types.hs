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
import Data.Hashable (Hashable(..))
import GHC.Generics (Generic)
import System.Posix.Directory.Foreign (DirType(..))
import System.Posix.FilePath (RawFilePath)
import Text.Regex.TDFA.ByteString (Regex)

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
-- The 'Regex' in 'RegEx' is a compiled DFA that does not have 'Eq' or
-- 'Hashable' instances.  We need the 'Regex' for comparison speed but need
-- 'Hashable' and 'Eq' instances for 'HashSet Ignore'; the 'ByteString' provides
-- both.
data Ignore = Literal Scope Target ByteString
            | RegEx Scope Target (ByteString, Regex)

instance Hashable Ignore where
    hashWithSalt s (Literal scope target bs) =
        s `hashWithSalt`
        (0 :: Int) `hashWithSalt`
        scope `hashWithSalt`
        target `hashWithSalt` bs

    -- ignore Regex
    hashWithSalt s (RegEx scope target (bs, _)) =
        s `hashWithSalt`
        (1 :: Int) `hashWithSalt`
        scope `hashWithSalt`
        target `hashWithSalt` bs

instance Eq Ignore where
    (Literal s1 t1 b1) == (Literal s2 t2 b2) = s1 == s2 && t1 == t2 && b1 == b2
    -- ignore Regex
    (RegEx s1 t1 (b1, _)) == (RegEx s2 t2 (b2, _)) =
        s1 == s2 && t1 == t2 && b1 == b2
    _ == _ = False

-- | Models filesystem contents for the current project.
data Tree = File (Path, Name)
          | Folder (Path, Name) [Tree]
          deriving (Show, Eq, Ord)
