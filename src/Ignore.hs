{-# LANGUAGE OverloadedStrings, OverloadedLists, ScopedTypeVariables #-}

module Ignore (
    ignore,
    ignored,
    readIgnores
) where

import Control.Applicative ((<$>))
import Control.Exception (handle, IOException)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Foldable (asum)
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Posix.FilePath ((</>), RawFilePath)
import Text.Regex.TDFA ((=~))
import Types (File, Ignore(..))

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

ignored :: Vector Ignore -> File -> Bool
ignored v (_, f) = V.all go v
    where
        go :: Ignore -> Bool
        go (Regex r) = not (f =~ r)
        go (Literal l) = f /= l
{-# INLINABLE ignored #-}

readIgnores :: RawFilePath -> IO (Vector Ignore)
readIgnores path = asum <$> V.mapM (readIgnore . (path </>)) dotfiles
    where
        readIgnore :: RawFilePath -> IO (Vector Ignore)
        readIgnore f = handle (\(_ :: IOException) -> return V.empty)
            (V.map ignore . V.fromList . B.lines <$> B.readFile (B.unpack f))

        dotfiles :: Vector ByteString
        dotfiles = [".gitignore", ".hgignore"]
{-# INLINABLE readIgnores #-}
