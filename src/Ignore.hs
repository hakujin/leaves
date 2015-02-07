{-# LANGUAGE OverloadedStrings, OverloadedLists, ScopedTypeVariables #-}

module Ignore (
    ignore,
    ignored,
    getIgnores
) where

import Control.Applicative ((<$>))
import Control.Exception (handle, IOException)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Foldable (all)
import System.Posix.FilePath ((</>), RawFilePath)
import Text.Regex.TDFA ((=~))
import Types (File, Ignore(..))
import Prelude hiding (all)

ignore :: RawFilePath -> Ignore
ignore bs =
    if '*' `B.elem` bs  || '?' `B.elem` bs
       then Regex (('^' `B.cons` B.concatMap go bs) `B.snoc` '$')
       else Literal bs
    where
        go :: Char -> ByteString
        go '*' = ".*"
        go '?' = "."
        go x = escape x

        escape :: Char -> ByteString
        escape c | c  `B.elem` regexChars = B.pack ['\\', c]
                | otherwise = B.singleton c
            where
                regexChars = "\\+()^$.{}]|"
{-# INLINABLE ignore #-}

ignored :: HashSet Ignore -> File -> Bool
ignored v (_, f) = all match v
    where
        match :: Ignore -> Bool
        match (Regex r) = not (f =~ r)
        match (Literal l) = f /= l
{-# INLINABLE ignored #-}

getIgnores :: RawFilePath -> IO (HashSet Ignore)
getIgnores p = S.fromList . concat <$> mapM (readIgnore . (p </>)) source
    where
        readIgnore :: RawFilePath -> IO [Ignore]
        readIgnore f = handle (\(_ :: IOException) -> return [])
                              (map ignore . B.lines <$> B.readFile (B.unpack f))

        source :: [ByteString]
        source = [".gitignore", ".hgignore"]
{-# INLINABLE getIgnores #-}
