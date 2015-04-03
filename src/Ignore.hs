{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Ignore (
    isAllowed,
    getIgnores,
    readIgnore
) where

import Control.Exception (handle, IOException)
import Control.Monad.State (State, get, modify, runState)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Foldable (all, foldrM)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import System.Posix.Directory.Foreign (DirType(..))
import Text.Regex.TDFA.ByteString (Regex, compile, execute)
import Text.Regex.TDFA.Common (CompOption(..), ExecOption(..))
import Prelude hiding (all)

import Types (Ignore(..), FileInfo, Name(..), Path(..), Scope(..), Target(..))
import Util (combine)

-- | Classify a pattern from a version control .ignore file for efficient
-- matching.
ignore :: Path -> ByteString -> Maybe Ignore
ignore (Path p) f =
    let ((t, s), f') = runState ((,) <$> target <*> scope) f
     in if '*' `B.elem` f'  || '?' `B.elem` f'
           then let r = ('^' `B.cons` B.concatMap go f') `B.snoc` '$'
                 in case compile compOption execOption r of
                         Right regex -> Just (RegEx s t (r, regex))
                         _           -> Nothing
           else Just (Literal s t f')
  where
    target :: State ByteString Target
    target = do
        b <- get
        if B.last b == '/'
           then modify B.init >> return Directory
           else return All

    scope :: State ByteString Scope
    scope = do
        b <- get
        if '/' `B.elem` b
           then modify (combine p) >> return Absolute
           else return Relative

    go :: Char -> ByteString
    go '*' = ".*"
    go '?' = "."
    go x = escape x

    escape :: Char -> ByteString
    escape c
        | c `B.elem` regexChars = B.pack ['\\', c]
        | otherwise = B.singleton c
      where
        regexChars = "\\+()^$.{}]|"

    compOption = CompOption { caseSensitive = True
                            , multiline = False
                            , rightAssoc = True
                            , newSyntax = False
                            , lastStarGreedy = True }

    execOption = ExecOption { captureGroups = False }

-- | Given the 'Ignore' Set, is this File/Folder allowed?
isAllowed :: HashSet Ignore -> FileInfo -> Bool
isAllowed v (d, (Path p, Name n)) = all (match d) v
  where
    match :: DirType -> Ignore -> Bool
    match (DirType 4) (Literal Relative Directory l) = n /= l
    match (DirType 4) (Literal Absolute Directory l) = p /= l
    match _ (Literal _ Directory _) = True
    match _ (Literal Relative All l) = n /= l
    match _ (Literal Absolute All l) = p /= l
    match (DirType 4) (RegEx Relative Directory (_, r)) = not (n =~ r)
    match (DirType 4) (RegEx Absolute Directory (_, r)) = not (p =~ r)
    match _ (RegEx _ Directory _) = True
    match _ (RegEx Relative All (_, r)) = not (n =~ r)
    match _ (RegEx Absolute All (_, r)) = not (p =~ r)

    (=~) :: ByteString -> Regex -> Bool
    (=~) b r = case execute r b of
                    Right (Just _) -> True
                    _              -> False

-- | Read and classify all 'Ignore' patterns from common version control
-- .ignore files in the current 'Path'.
getIgnores :: Path -> [FileInfo] -> IO (HashSet Ignore)
getIgnores p = foldrM step mempty
  where
    step :: FileInfo -> HashSet Ignore -> IO (HashSet Ignore)
    step (DirType 8, (_, n)) i | n `elem` ns = (<> i) <$> readIgnore p n
    step _ i = return i

    ns :: [Name]
    ns = map Name [".gitignore", ".hgignore"]

-- | Read and classify all 'Ignore' patterns from the specified 'Path'.
readIgnore :: Path -> Name -> IO (HashSet Ignore)
readIgnore path@(Path p) (Name n) =
    handle (\(_ :: IOException) -> return mempty) $
        S.fromList . mapMaybe (ignore path) . filter isPattern . B.lines <$>
            (B.readFile . B.unpack . combine p) n
  where
    isPattern :: ByteString -> Bool
    isPattern b = not (B.null b) && B.head b /= '#'
