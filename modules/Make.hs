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

import Yi
import qualified Yi.Rope as R
import Yi.Utils
import qualified Yi.Keymap.Vim.Common as V
import qualified Yi.Keymap.Vim.Ex.Types as V
import qualified Yi.Keymap.Vim.Ex.Commands.Common as V

exMake :: V.EventString -> Maybe V.ExCommand
exMake "make" = Just (V.impureExCommand{V.cmdAction = YiA cmdAction})
exMake _ = Nothing

cmdAction :: YiM ()
cmdAction = do
    printMsg "Launching make process"
    x <- ask
    void . io . forkIO $ do
        (code, out, err) <- readProcessWithExitCode "make" [] ""
        let messages = mapMaybe parseCompilerMessage (lines (out <> "\n" <> err))
            messagesByFile :: M.Map FilePath [CompilerMessage]
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
                        printMsg "Yay!"
                        withGivenBuffer ref $ do
                            delOverlaysOfOwnerB "make"
                            case M.lookup filename messagesByFile of
                                Just bufMessages -> do
                                    insertN "*"
                                    mapM_
                                        (addOverlayB <=< messageToOverlayB)
                                        bufMessages
                                _ -> return ()
                    _ -> return ()
                case code of
                    ExitSuccess -> printMsg "Make finished successfully."
                    _ -> printMsg ("Make failed, " <>
                            (T.pack (show (length messages))) <> " errors.")
        yiOutput x MustRefresh [EditorA action]

parseCompilerMessage :: String -> Maybe CompilerMessage
parseCompilerMessage s = 
    case R.split (== ':') (R.fromString s) of
        (filename : _) -> Just (CompilerMessage (R.toString filename) 5 5 5 9)
        _ -> Nothing

data CompilerMessage = CompilerMessage
    { cmFilePath :: !FilePath
    , cmStartLine :: !Int
    , cmStartColumn :: !Int
    , cmEndLine :: !Int
    , cmEndColumn :: !Int
    }

messageToOverlayB :: CompilerMessage -> BufferM Overlay
messageToOverlayB (CompilerMessage _ l1 c1 l2 c2) = savingPointB $ do
    moveToLineColB l1 c1
    p1 <- pointB
    moveToLineColB l2 c2
    p2 <- pointB
    return (mkOverlay "make" (mkRegion p1 p2) errorStyle)
