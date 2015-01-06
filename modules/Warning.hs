{-# LANGUAGE DeriveGeneric #-}

module Warning
    ( Warning (..)
    , parseWarning
    ) where

import Control.Applicative
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
    fn <- filename
    _ <- P.char ':'
    l <- number
    _ <- P.char ':'
    c <- number
    _ <- P.char ':'
    _ <- P.anyChar
    return (Warning fn l c l (c + 1))

line :: P.GenParser Char () Warning
line = do
    fn <- filename
    _ <- P.char ':'
    l <- number
    _ <- P.char ':'
    _ <- P.many P.anyChar
    return (Warning fn l 1 l (-1))

onelineSpan :: P.GenParser Char () Warning
onelineSpan = do
    fn <- filename
    _ <- P.char ':'
    l <- number
    _ <- P.char ':'
    c1 <- number
    _ <- P.char '-'
    c2 <- number
    _ <- P.char ':'
    _ <- P.many P.anyChar
    return (Warning fn l c1 l c2)

multilineSpan :: P.GenParser Char () Warning
multilineSpan = do
    fn <- filename
    _ <- P.char ':'
    (l1, c1) <- lineCol
    _ <- P.char '-'
    (l2, c2) <- lineCol
    _ <- P.many P.anyChar
    return (Warning fn l1 c1 l2 c2)

filename :: P.GenParser Char () FilePath
filename = (:) <$> P.noneOf " :" <*> P.many1 (P.noneOf ":")

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