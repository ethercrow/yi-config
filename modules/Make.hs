{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO:
-- 
--  * fuzzy-open dialog for error list
--  * Somehow mark tabs with errors. Maybe with color or with a '!'.
--  * :make1 and :wm1 commands for compiling just current file

-- Bugs:
--
--  * If user runs :make first and then opens a file for which errors
--    were emitted, there is no error highlighting in that new buffer.
--  * Make process is leaked on editor exit

module Make
    ( debug
    , errorCountE
    , errorCountB
    , exMake
    , exMakePrgOption
    , guessMakePrg
    , jumpToNextErrorInCurrentBufferY
    , jumpToNextErrorY
    , showErrorE
    , insertErrorMessageE
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.State (gets)
import Control.Monad.Reader
import Data.Binary
import Data.Default
import Data.Foldable (find, toList)
import Data.List (isSuffixOf, sortBy)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Ord (comparing, Down (..))
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Typeable
import qualified Data.Vector as V
import Lens.Micro.Platform
import System.Directory
import System.Exit
import System.FilePath
import System.Process

import Yi
import qualified Yi.Keymap.Vim.Common as V
import qualified Yi.Keymap.Vim.Ex.Types as V
import qualified Yi.Keymap.Vim.Ex.Commands.Common as V
import qualified Yi.Rope as R
import Yi.Types
import Yi.Utils
import Yi.Window (width)

import Text.Warning
import YiWarning

overlayName :: R.YiString
overlayName = "make"

errorCountE :: EditorM Int
errorCountE = do
    WarningStorage warnings <- getEditorDyn
    return (getSum (foldMap (Sum . V.length) warnings))

errorCountB :: BufferM Int
errorCountB = S.size <$> getOverlaysOfOwnerB overlayName

data Scope = CurrentBuffer | Global

jumpToNextErrorInCurrentBufferY :: Direction -> YiM ()
jumpToNextErrorInCurrentBufferY = jumpToNextErrorY' CurrentBuffer

jumpToNextErrorY :: Direction -> YiM ()
jumpToNextErrorY = jumpToNextErrorY' Global

jumpToNextErrorY' :: Scope -> Direction -> YiM ()
jumpToNextErrorY' scope dir = do
    p <- withCurrentBuffer pointB
    let (comp, behind) = case dir of
            Forward ->
                (comparing overlayBegin, (<= p))
            Backward ->
                (comparing (Down . overlayBegin), (>= p))
        focusOnOverlayE o = do
            let beginPoint = markPoint (overlayBegin o)
            withCurrentBuffer (moveTo beginPoint)
            showOverlayMessageE o
    overlays <-
        fmap
            (sortBy comp . toList)
            (withCurrentBuffer (getOverlaysOfOwnerB overlayName))
    case (overlays, scope) of
        ([], CurrentBuffer) -> printMsg "No errors in this file"
        ([], Global) -> do
            WarningStorage warnings <- getEditorDyn
            case M.toList warnings of
                [] -> printMsg "No errors"
                (FileBuffer filepath, fileWarnings) : _ -> do
                    Right _ <- editFile filepath
                    firstOverlay <- withCurrentBuffer $ do
                        V.mapM_
                            (addOverlayB <=< messageToOverlayB errorStyle)
                            fileWarnings
                        messageToOverlayB errorStyle (V.head fileWarnings)
                    focusOnOverlayE firstOverlay
                (MemBuffer name, _) : _ ->
                    error ("unexpected warning in membuffer " <> T.unpack name)
        (firstOverlay : _, _) -> do
            let overlaysBelow =
                    dropWhile
                        (behind . markPoint . overlayBegin)
                        overlays
            focusOnOverlayE (case overlaysBelow of
                        [] -> firstOverlay
                        o : _ -> o)

debug :: EditorM ()
debug = do
    ws :: WarningStorage <- getEditorDyn
    withCurrentBuffer $ insertN (R.fromString (show ws))

showOverlayMessageE :: MonadEditor m => Overlay -> m ()
showOverlayMessageE overlay = do
    winWidth <- fmap width (use currentWindowA)
    printMsgs
        (fmap
            (T.justifyLeft (winWidth - 1) ' ' . R.toText)
            (R.lines (overlayAnnotation overlay)))

showErrorE :: EditorM ()
showErrorE = do
    maybeOverlayUnderCursor <- withCurrentBuffer maybeOverlayUnderCursorB
    maybe (return ()) showOverlayMessageE maybeOverlayUnderCursor

maybeOverlayUnderCursorB :: BufferM (Maybe Overlay)
maybeOverlayUnderCursorB = do
    p <- pointB
    overlays <- getOverlaysOfOwnerB "make"
    return (find (isPointInsideOverlay p) overlays)

insertErrorMessageE :: EditorM ()
insertErrorMessageE = do
    mo <- withCurrentBuffer maybeOverlayUnderCursorB
    maybe (return ()) (withCurrentBuffer . insertN . overlayAnnotation) mo

exMake :: V.EventString -> Maybe V.ExCommand
exMake "make" = Just (V.impureExCommand{V.cmdAction = YiA make})
exMake "wm" = Just (V.impureExCommand{V.cmdAction = YiA (viWrite >> make)})
exMake _ = Nothing

exMakePrgOption :: V.EventString -> Maybe V.ExCommand
exMakePrgOption = V.parseTextOption "makeprg" $ \case
    V.TextOptionAsk -> EditorA $ do
        makePrg <- getEditorDyn
        printMsg $ "makeprg = " <> unMakePrg makePrg
    (V.TextOptionSet makePrg) -> EditorA (putEditorDyn (MakePrg makePrg))

newtype MakePrg = MakePrg { unMakePrg :: T.Text }
    deriving (Show, Typeable)

instance Binary MakePrg where
    get = fmap (MakePrg . TE.decodeUtf8) get
    put (MakePrg text)  = put (TE.encodeUtf8 text)

instance Default MakePrg where
    def = MakePrg "make"

instance YiVariable MakePrg


make :: YiM ()
make = do
    makeprg <- withEditor getEditorDyn
    let _cmd : args =
            -- TODO: T.words is insufficient to correctly split
            --       something like "nix-shell --command 'cabal install'"
            case T.words (unMakePrg makeprg) of
                [] -> error "empty makeprg"
                ws -> fmap T.unpack ws
        maybeCustomMakeDir = case args of
            "-C" : d : _ -> Just d
            _ -> Nothing
        procSpec = shell (T.unpack (unMakePrg makeprg))
    printMsg ("Launching " <> unMakePrg makeprg)
    x <- ask
    void . io . forkIO $ do
        possiblyException <- try $ do
            (code, out, err) <- readCreateProcessWithExitCode procSpec ""
            ws@(WarningStorage warningsByBuffer) <-
                fixPathsInBufferIds maybeCustomMakeDir (parseWarningStorage (out <> "\n" <> err))
            let style = if code == ExitSuccess then hintStyle else errorStyle
                action = do
                    putEditorDyn ws
                    bufs <- fmap M.toList (gets buffers)
                    forM_ bufs $ \(ref, buf) ->
                        withGivenBuffer ref . retroactivelyAtSavePointB $ do
                            delOverlaysOfOwnerB "make"
                            case M.lookup (buf ^. identA) warningsByBuffer of
                                Just warnings ->
                                        V.mapM_
                                            (addOverlayB <=< messageToOverlayB style)
                                            warnings
                                _ -> return ()
                    printMsg (unMakePrg makeprg <> case code of
                        ExitSuccess -> " finished successfully."
                        ExitFailure f ->
                            let errorCount = 42 -- length ws
                            in " failed with code " <> showT f <> ", " <> showT errorCount <> " errors")
            yiOutput x MustRefresh [EditorA action]
        case possiblyException of
            Left e -> yiOutput x MustRefresh [EditorA (printMsg (T.pack (show (e :: SomeException))))]
            _ -> return ()

guessMakePrg :: YiM ()
guessMakePrg = do
    files <- io (getDirectoryContents ".")
    let makePrg =
            if "Makefile" `elem` files then "make"
            else if "Shakefile" `elem` files then "shake"
            else if any (".cabal" `isSuffixOf`) files then "cabal build"
            else "make"
    withEditor (putEditorDyn (MakePrg makePrg))

showT :: Show a => a -> T.Text
showT = T.pack . show