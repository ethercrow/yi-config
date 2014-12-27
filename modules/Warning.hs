{-# LANGUAGE DeriveGeneric #-}

module Warning
    ( Warning (..)
    , parseWarning
    ) where

import Data.Binary
import GHC.Generics

import qualified Text.ParserCombinators.Parsec as P

data Warning = Warning
    { cmFilePath :: !FilePath
    , cmStartLine :: !Int
    , cmStartColumn :: !Int
    , cmEndLine :: !Int
    , cmEndColumn :: !Int
    } deriving (Generic, Show)

parseWarning :: String -> Maybe Warning
parseWarning s =
    either (const Nothing) Just (P.parse (P.choice errorParsers) "" s)
    where
        errorParsers = [P.try multilineSpan, P.try onelineSpan, P.try point, line]

point :: P.GenParser Char () Warning
point = do
    filename <- P.many1 (P.noneOf ":")
    _ <- P.char ':'
    l <- number
    _ <- P.char ':'
    c <- number
    _ <- P.char ':'
    _ <- P.anyChar
    return (Warning filename l c l (c + 1))

line :: P.GenParser Char () Warning
line = do
    filename <- P.many1 (P.noneOf ":")
    _ <- P.char ':'
    l <- number
    _ <- P.char ':'
    _ <- P.many P.anyChar
    return (Warning filename l 1 l (-1))

onelineSpan :: P.GenParser Char () Warning
onelineSpan = do
    filename <- P.many1 (P.noneOf ":")
    _ <- P.char ':'
    l <- number
    _ <- P.char ':'
    c1 <- number
    _ <- P.char '-'
    c2 <- number
    _ <- P.char ':'
    _ <- P.many P.anyChar
    return (Warning filename l c1 l c2)

multilineSpan :: P.GenParser Char () Warning
multilineSpan = do
    filename <- P.many1 (P.noneOf ":")
    _ <- P.char ':'
    (l1, c1) <- lineCol
    _ <- P.char '-'
    (l2, c2) <- lineCol
    _ <- P.many P.anyChar
    return (Warning filename l1 c1 l2 c2)

number :: P.GenParser Char () Int
number = fmap read (P.many1 P.digit)

lineCol :: P.GenParser Char () (Int, Int)
lineCol = do
    _ <- P.char '('
    l <- number
    _ <- P.char ','
    c <- number
    _ <- P.char ')'
    return (l, c)

instance Binary Warning