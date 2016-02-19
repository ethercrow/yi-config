{-# LANGUAGE OverloadedStrings #-}

module IdrisIDESexp
    ( serializeCommand
    , hGetSExp
    ) where

import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Data.Monoid
import Text.Printf
import System.Environment
import System.IO
import System.Process

import qualified Idris.IdeMode as IDE
import IdrisIDESexpCopyPasteFromIdris

commandToSExp :: IDE.IdeModeCommand -> IDE.SExp
commandToSExp (IDE.LoadFile f Nothing) =
    IDE.SexpList [IDE.SymbolAtom "load-file", IDE.StringAtom f]
commandToSExp (IDE.LoadFile f (Just line)) =
    IDE.SexpList
        [ IDE.SymbolAtom "load-file"
        , IDE.StringAtom f
        , IDE.IntegerAtom (fromIntegral line)
        ]
commandToSExp (IDE.Interpret code) =
    IDE.SexpList
        [ IDE.SymbolAtom "interpret"
        , IDE.StringAtom code
        ]

serializeCommand :: IDE.IdeModeCommand -> String
serializeCommand cmd = 
    getHexLength s <> s
    where
    s = IDE.sExpToString (IDE.SexpList [commandToSExp cmd, IDE.IntegerAtom 42])

getHexLength :: String -> String
getHexLength s = printf "%06x" (1 + (length s))

hGetSExp :: Handle -> IO IDE.SExp
hGetSExp h = do
    line <- hGetLine h
    let messageLength = read ("0x" <> take 6 line)
        possiblyUnfinishedMessage = drop 6 line
    wholeMessage <-
        if messageLength == length possiblyUnfinishedMessage
        then pure possiblyUnfinishedMessage
        else do
            epilogue <- B.hGet h (messageLength - length possiblyUnfinishedMessage - 1)
            pure (possiblyUnfinishedMessage <> (B.unpack ("\n" <> epilogue)))
    case receiveString wholeMessage of
        Right sexp -> pure sexp
        Left err -> do
            putStrLn wholeMessage
            error (show err)