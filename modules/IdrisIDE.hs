{-# LANGUAGE OverloadedStrings #-}

module IdrisIDE
    ( startIdrisIDE
    , exIdris 
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.State (gets)
import Data.Binary
import Data.Default
import qualified Data.ByteString.Char8 as B
import Data.Monoid
import qualified Data.Text as T
import Text.Printf
import System.Environment
import System.IO
import System.Process

import qualified Idris.IdeMode as IDE
import IdrisIDESexp

import Yi
import Yi.Types
import qualified Yi.Keymap.Vim.Common as Vim
import qualified Yi.Keymap.Vim.Ex as Vim
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Vim
import qualified Yi.Rope as R

exIdris :: Vim.EventString -> Maybe Vim.ExCommand
exIdris "idris-load-current-file" =
    Just (Vim.impureExCommand{Vim.cmdAction = YiA loadCurrentFileY})
exIdris "idris-interpret-line" =
    Just (Vim.impureExCommand{Vim.cmdAction = YiA interpretLineY})
exIdris _ = Nothing

data IdrisProcess = IdrisProcess
    { hIn :: Handle
    , hOut :: Handle
    , process :: ProcessHandle
    }

instance Binary IdrisProcess where
    put _ = error "put IdrisProcess"
    get = error "get IdrisProcess"

instance Default IdrisProcess where
    def = error "def IdrisProcess"

instance YiVariable IdrisProcess

startIdrisIDE :: YiM ()
startIdrisIDE = do
    (Just idrisIn, Just idrisOut, Nothing, p) <- liftBase $
        createProcess (shell "idris --ide-mode")
            {std_out = CreatePipe, std_in = CreatePipe}
    putEditorDyn (IdrisProcess idrisIn idrisOut p)

loadCurrentFileY :: YiM ()
loadCurrentFileY = do
    IdrisProcess hIn hOut _ <- getEditorDyn
    currentFilename <- withCurrentBuffer (gets identString)
    void $ loadFile hIn hOut (T.unpack currentFilename)

interpretLineY :: YiM ()
interpretLineY = do
    IdrisProcess hIn hOut _ <- getEditorDyn
    line <- withCurrentBuffer readLnB
    void $ interpret hIn hOut (R.toString line)

loadFile :: Handle -> Handle -> FilePath -> YiM IDE.SExp
loadFile hin hout filename = do
    liftBase $ do
        hPutStrLn hin (serializeCommand (IDE.LoadFile filename Nothing))
        hFlush hin
    getReply hout

interpret :: Handle -> Handle -> String -> YiM IDE.SExp
interpret hin hout code = do
    liftBase $ do
        hPutStrLn hin (serializeCommand (IDE.Interpret code))
        hFlush hin
    getReply hout

getReply :: Handle -> YiM IDE.SExp
getReply hout =
    let go = do
            sexp <- liftBase (hGetSExp hout)
            case sexp of
                IDE.SexpList [(IDE.SymbolAtom "write-string"), x, _] -> do
                    printMsg (T.pack (IDE.sExpToString x))
                    go
                IDE.SexpList [(IDE.SymbolAtom "set-prompt"), x, _] -> do
                    printMsg ("> " <> T.pack (IDE.sExpToString x))
                    go
                IDE.SexpList [(IDE.SymbolAtom "output"), x, _] -> do
                    printMsg ("INFO " <> T.pack (IDE.sExpToString x))
                    go
                IDE.SexpList [(IDE.SymbolAtom "warning"), x, _] -> do
                    printMsg ("WARN " <> T.pack (IDE.sExpToString x))
                    pure x
                IDE.SexpList [(IDE.SymbolAtom "return"), x, _] -> do
                    printMsg ("RET " <> T.pack (IDE.sExpToString x))
                    pure x
                x -> do
                    printMsg (T.pack (show x))
                    go
    in go