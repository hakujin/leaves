{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ignore (
    ignore,
    ignored,
    Ignored
) where

import Text.Regex.TDFA ((=~))

newtype Ignored = Regex String deriving (Show, Eq)

ignored :: String -> Ignored
ignored = Ignored . globToRegex

ignore :: [Ignored] -> String -> Bool
ignore [] _ = True
ignore (Ignored r:rs) s = if (r =~ s) then ignore rs s else False

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
