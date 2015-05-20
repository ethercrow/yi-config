{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Warning
    ( Warning (..)
    , parseWarnings
    ) where

import Control.Applicative
import Data.Binary
import Data.Char
import Data.Monoid
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Text.ParserCombinators.Parsec as P

type Parser a = P.GenParser Char () a

data Line
    = StarterLine Warning
    | WarningTextLine T.Text
    | GarbageLine T.Text
    | EmptyLine
    deriving Show

data Warning = Warning
    { cmFilePath :: !FilePath
    , cmStartLine :: !Int
    , cmStartColumn :: !Int
    , cmEndLine :: !Int
    , cmEndColumn :: !Int
    , cmMessage :: !T.Text
    } deriving (Eq, Generic, Show)

parseWarnings :: String -> [Warning]
parseWarnings input =
    go Nothing [] classifiedLines
    where
        go Nothing ws [] = reverse ws
        go (Just w) ws [] = reverse (w : ws)
        go (Just w) ws (WarningTextLine t : rest) =
            go (Just (appendMsg t w)) ws rest
        go (Just w) ws rest = go Nothing (w : ws) rest
        go Nothing ws (StarterLine w : rest) = go (Just w) ws rest
        go _ ws (_ : rest) = go Nothing ws rest
        classifiedLines = map parseLine (lines input)
        parseLine "" = EmptyLine
        parseLine s@(c : _) | isSpace c = WarningTextLine (T.pack s)
        parseLine s = either
            (const (GarbageLine (T.pack s)))
            StarterLine
            (P.parse (P.choice errorParsers) "" s)
        errorParsers =
            [ P.try multilineSpan
            , P.try onelineSpan
            , P.try point
            , line
            ]
        appendMsg :: T.Text -> Warning -> Warning
        appendMsg msg2 (Warning fn l1 c1 l2 c2 msg) =
            Warning fn l1 c1 l2 c2 (msg <> "\n" <> msg2)

point :: P.GenParser Char () Warning
point = do
    fn <- filename
    _ <- P.char ':'
    l <- number
    _ <- P.char ':'
    c <- number
    _ <- P.char ':'
    msg <- message
    return (Warning fn l c l (c + 1) msg)

line :: P.GenParser Char () Warning
line = do
    fn <- filename
    _ <- P.char ':'
    l <- number
    _ <- P.char ':'
    msg <- message
    return (Warning fn l 1 l (-1) msg)

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
    msg <- message
    return (Warning fn l c1 l c2 msg)

multilineSpan :: P.GenParser Char () Warning
multilineSpan = do
    fn <- filename
    _ <- P.char ':'
    (l1, c1) <- lineCol
    _ <- P.char '-'
    (l2, c2) <- lineCol
    msg <- message
    return (Warning fn l1 c1 l2 c2 msg)

filename :: P.GenParser Char () FilePath
filename = (:) <$> P.noneOf " :\t" <*> P.many1 (P.noneOf ":\t")

message :: Parser T.Text
message = fmap T.pack (P.many P.anyChar)

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

instance Binary Warning where
    put (Warning fn l1 c1 l2 c2 msg) = do
        put (TE.encodeUtf8 msg)
        put fn
        put l1 >> put c1
        put l2 >> put c2
    get = do
        msg <- fmap TE.decodeUtf8 get
        Warning <$> get <*> get <*> get <*> get <*> get <*> pure msg