{-# LANGUAGE OverloadedStrings #-}

module Ignore (
    ignore,
    allowed,
    getIgnores
) where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Foldable (all, foldrM)
import Data.Monoid ((<>))
import System.Posix.Directory.Foreign (DirType(..))
import Text.Regex.TDFA ((=~))
import Prelude hiding (all)

import Types (Ignore(..), FileInfo, Name(..), Path(..), Scope(..), Target(..))
import Util ((</>))

--------------------------------------------------------------------------------
ignore :: Path -> ByteString -> Ignore
ignore (Path p) f =
    let (t, f') = target f
        (s, f'') = scope f'
    in if '*' `B.elem` f''  || '?' `B.elem` f''
        then Regex s t (('^' `B.cons` B.concatMap go f'') `B.snoc` '$')
        else Literal s t f''
  where
    target :: ByteString -> (Target, ByteString)
    target b = if B.last b == '/' then (Directory, B.init b) else (All, b)

    scope :: ByteString -> (Scope, ByteString)
    scope b = if '/' `B.elem` B.init b
                    then (Absolute, p <> "/" <> b)
                    else (Relative, b)

    go :: Char -> ByteString
    go '*' = ".*"
    go '?' = "."
    go x = escape x

    escape :: Char -> ByteString
    escape c
        | c  `B.elem` regexChars = B.pack ['\\', c]
        | otherwise = B.singleton c
      where
        regexChars = "\\+()^$.{}]|"

--------------------------------------------------------------------------------
allowed :: HashSet Ignore -> FileInfo -> Bool
allowed v (d, ((Path p), (Name n))) = all (match d) v
  where
    match :: DirType -> Ignore -> Bool
    match (DirType 4) (Literal Relative Directory l) = n /= l
    match (DirType 4) (Literal Absolute Directory l) = p /= l
    match _ (Literal _ Directory _) = True
    match _ (Literal Relative All l) = n /= l
    match _ (Literal Absolute All l) = p /= l
    match (DirType 4) (Regex Relative Directory r) = not (n =~ r)
    match (DirType 4) (Regex Absolute Directory r) = not (p =~ r)
    match _ (Regex _ Directory _) = True
    match _ (Regex Relative All r) = not (n =~ r)
    match _ (Regex Absolute All r) = not (p =~ r)

--------------------------------------------------------------------------------
getIgnores :: Path -> [FileInfo] -> IO (HashSet Ignore)
getIgnores p = foldrM step S.empty
  where
    step :: FileInfo -> HashSet Ignore -> IO (HashSet Ignore)
    step (DirType 8, (_, n)) a
        | n `elem` dotIgnore = (`S.union` a) <$> readIgnore (p </> n)
    step _ a = return a

    dotIgnore :: [Name]
    dotIgnore = map Name [".gitignore", ".hgignore"]

--------------------------------------------------------------------------------
readIgnore :: Path -> IO (HashSet Ignore)
readIgnore path@(Path p) = S.fromList .
    map (ignore path) . filter whitespace . B.lines <$> B.readFile (B.unpack p)
  where
    whitespace :: ByteString -> Bool
    whitespace b = not (B.null b) && B.head b /= '#'
