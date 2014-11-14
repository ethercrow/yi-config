{-# LANGUAGE OverloadedStrings #-}

module Make where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.Function (on)
import Data.List (sortBy, groupBy)
import Data.Maybe (mapMaybe)
import Data.Monoid
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import qualified Data.Text as T
import System.Exit
import System.Process
import qualified Text.ParserCombinators.Parsec as P

import Yi
import qualified Yi.Rope as R
import Yi.Utils
import System.CanonicalizePath
import qualified Yi.Keymap.Vim.Common as V
import qualified Yi.Keymap.Vim.Ex.Types as V
import qualified Yi.Keymap.Vim.Ex.Commands.Common as V

exMake :: V.EventString -> Maybe V.ExCommand
exMake "make" = Just (V.impureExCommand{V.cmdAction = YiA cmdAction})
exMake _ = Nothing

cmdAction :: YiM ()
cmdAction = do
    let makeprg = "make"
    printMsg "Launching make process"
    x <- ask
    void . io . forkIO $ do
        (code, out, err) <- readProcessWithExitCode makeprg [] ""
        let messagesWithPossiblyRelativePaths =
                mapMaybe parseCompilerMessage (lines (out <> "\n" <> err))
        messages <-
            mapM absolutizePathInMessage messagesWithPossiblyRelativePaths
        let messagesByFile :: M.Map FilePath [CompilerMessage]
            messagesByFile =
                M.fromList
                    (fmap
                        (\msgs@(msg : _) -> (cmFilePath msg, msgs))
                        (groupBy
                            ((==) `on` cmFilePath)
                            (sortBy (comparing cmFilePath) messages)))
            action = do
                bufs <- fmap M.toList (gets buffers)
                forM_ bufs $ \(ref, buf) -> case buf ^. identA of
                    FileBuffer filename -> do
                        withGivenBuffer ref $ do
                            delOverlaysOfOwnerB "make"
                            case M.lookup filename messagesByFile of
                                Just bufMessages -> do
                                    mapM_
                                        (addOverlayB <=< messageToOverlayB)
                                        bufMessages
                                _ -> return ()
                    _ -> return ()
                case code of
                    ExitSuccess -> printMsg "Make finished successfully."
                    _ -> printMsg ("Make failed, " <>
                            (T.pack (show (length messages))) <>
                            " errors:" <>
                            T.unlines (fmap (T.pack . show) messages))
        yiOutput x MustRefresh [EditorA action]

parseCompilerMessage :: String -> Maybe CompilerMessage
parseCompilerMessage s =
    either (const Nothing) Just (P.parse (P.choice errorParsers) "" s)
    where
        errorParsers = [P.try multilineSpan, P.try onelineSpan, point, line]

point :: P.GenParser Char () CompilerMessage
point = do
    filename <- P.many1 (P.noneOf ":")
    _ <- P.char ':'
    l <- number
    _ <- P.char ':'
    c <- number
    _ <- P.char ':'
    _ <- P.anyChar
    return (CompilerMessage filename l c l (c + 1))

line :: P.GenParser Char () CompilerMessage
line = do
    filename <- P.many1 (P.noneOf ":")
    _ <- P.char ':'
    l <- number
    _ <- P.char ':'
    _ <- P.anyChar
    return (CompilerMessage filename l 0 l 1)

onelineSpan :: P.GenParser Char () CompilerMessage
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
    return (CompilerMessage filename l c1 l c2)

multilineSpan :: P.GenParser Char () CompilerMessage
multilineSpan = do
    filename <- P.many1 (P.noneOf ":")
    _ <- P.char ':'
    (l1, c1) <- lineCol
    _ <- P.char '-'
    (l2, c2) <- lineCol
    _ <- P.many P.anyChar
    return (CompilerMessage filename l1 c1 l2 c2)

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

data CompilerMessage = CompilerMessage
    { cmFilePath :: !FilePath
    , cmStartLine :: !Int
    , cmStartColumn :: !Int
    , cmEndLine :: !Int
    , cmEndColumn :: !Int
    } deriving Show

messageToOverlayB :: CompilerMessage -> BufferM Overlay
messageToOverlayB (CompilerMessage _ l1 c1 l2 c2) = savingPointB $ do
    moveToLineColB l1 (c1 - 1)
    p1 <- pointB
    moveToLineColB l2 c2
    p2 <- pointB
    return (mkOverlay "make" (mkRegion p1 p2) errorStyle)

absolutizePathInMessage :: CompilerMessage -> IO CompilerMessage
absolutizePathInMessage msg@CompilerMessage{cmFilePath = path} = do
    absPath <- normalisePath path
    return msg{cmFilePath = absPath}

