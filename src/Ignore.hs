{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Ignore (
    ignore,
    ignored,
    readIgnores
) where

import Control.Applicative ((<$>))
import Control.Exception (catch, IOException)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
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

ignored :: [Ignore] -> File -> Bool
ignored [] _ = True
ignored (Regex r:rs) p@(_, f) = not (f =~ r) && ignored rs p
ignored (Literal l:rs) p@(_, f) = (f /= l) && ignored rs p
{-# INLINABLE ignored #-}

readIgnores :: [RawFilePath] -> RawFilePath -> IO [Ignore]
readIgnores is path = concat <$> mapM (readIgnore . (path </>)) is
    where
        readIgnore :: RawFilePath -> IO [Ignore]
        readIgnore f = catch (map ignore . B.lines <$> B.readFile (B.unpack f))
                             (\(_ :: IOException) -> return [])
{-# INLINABLE readIgnores #-}
