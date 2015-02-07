{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}

module Ignore (
    ignore,
    ignored,
    readIgnores,
    Ignore
) where

import Control.Applicative ((<$>))
import System.FilePath ((</>))
import Control.Exception (catch, IOException)
import Text.Regex.TDFA ((=~))

newtype Ignore = Ignore String deriving (Show, Eq)

ignore :: String -> Ignore
ignore = Ignore . globToRegex

ignored :: [Ignore] -> String -> Bool
ignored [] _ = True
ignored (Ignore r:rs) s = not (s =~ r) && ignored rs s

readIgnores :: [FilePath] -> FilePath -> IO [Ignore]
readIgnores is path = concat <$> mapM (readIgnore . (path </>)) is
    where
        readIgnore :: FilePath -> IO [Ignore]
        readIgnore f = catch (map ignore . lines <$> readFile f)
                             (\(_ :: IOException) -> return [])

globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' "" = ""
globToRegex' ('*':cs) = ".*" ++ globToRegex' cs
globToRegex' ('?':cs) = '.' : globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs)     = '['  :  c : charClass cs
globToRegex' ('[':_)        = error "unterminated character class"
globToRegex' (c:cs) = escape c ++ globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where
        regexChars = "\\+()^$.{}]|"

charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"
