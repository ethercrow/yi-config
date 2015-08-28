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

module Make
    ( debug
    , errorCountE
    , errorCountB
    , exMake
    , exMakePrgOption
    , guessMakePrg
    , jumpToNextErrorE
    , showErrorE
    , insertErrorMessageE
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Lens hiding (Action)
import Control.Monad
import Control.Monad.State (gets)
import Control.Monad.Reader
import Data.Binary
import Data.Default
import Data.Foldable (Foldable, find, foldMap, toList)
import Data.List (isSuffixOf, sortBy)
import Data.Monoid
import Data.Ord (comparing, Down (..))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Tuple (swap)
import Data.Typeable
import qualified Data.Vector as V
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

import Warning

overlayName :: R.YiString
overlayName = "make"

errorCountE :: EditorM Int
errorCountE = do
    WarningStorage warnings <- getEditorDyn
    return (getSum (foldMap (Sum . V.length) warnings))

errorCountB :: BufferM Int
errorCountB = do
    overlays <- getOverlaysOfOwnerB overlayName
    return (lengthOf folded overlays)

jumpToNextErrorE :: Direction -> EditorM ()
jumpToNextErrorE dir = do
    p <- withCurrentBuffer pointB
    let (comp, behind) = case dir of
            Forward ->
                (comparing overlayBegin, (<= p))
            Backward ->
                (comparing (Down . overlayBegin), (>= p))
    overlays <-
        fmap
            (sortBy comp . toList)
            (withCurrentBuffer (getOverlaysOfOwnerB overlayName))
    case overlays of
        [] -> printMsg "No errors in this file"
        firstOverlay : _ -> do
            let overlaysBelow =
                    dropWhile
                        (behind . markPoint . overlayBegin)
                        overlays
                dst = (markPoint . overlayBegin)
                    (case overlaysBelow of
                        [] -> firstOverlay
                        o : _ -> o)
            withCurrentBuffer (moveTo dst)
            showOverlayMessageE firstOverlay

debug :: EditorM ()
debug = do
    ws :: WarningStorage <- getEditorDyn
    withCurrentBuffer $ insertN (R.fromString (show ws))

showOverlayMessageE :: Overlay -> EditorM ()
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

newtype WarningStorage = WarningStorage (M.Map BufferId (V.Vector Warning))
    deriving (Typeable, Show)

instance Default WarningStorage where
    def = WarningStorage def

instance Binary WarningStorage where
    get = fmap (WarningStorage . fmap V.fromList) get
    put (WarningStorage ws) = put (fmap V.toList ws)

instance YiVariable WarningStorage

make :: YiM ()
make = do
    makeprg <- withEditor getEditorDyn
    let cmd : args =
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
                        ExitFailure f -> " failed with code " <> T.pack (show f))
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

parseWarningStorage :: String -> WarningStorage
parseWarningStorage =
    WarningStorage . M.mapKeysMonotonic FileBuffer .
        mapFromValues cmFilePath . parseWarnings

mapFromValues :: (Foldable f, Applicative t, Monoid (t v), Ord k)
    => (v -> k) -> f v -> M.Map k (t v)
mapFromValues keyFun values =
    M.fromListWith (<>) (fmap (\v -> (keyFun v, pure v)) (toList values))

messageToOverlayB :: (UIStyle -> Style) -> Warning -> BufferM Overlay
messageToOverlayB style (Warning _ l1 c1 l2 c2 msg) = savingPointB $ do
    moveToLineColB l1 (c1 - 1)
    p1 <- pointB
    if c2 >= 0
    then moveToLineColB l2 c2
    else moveToLineColB l2 0 >> moveToEol
    p2 <- pointB
    return (mkOverlay "make" (mkRegion p1 p2) style (R.fromText msg))

fixPathsInBufferIds :: Maybe FilePath -> WarningStorage -> IO WarningStorage
fixPathsInBufferIds maybeCustomMakeDir (WarningStorage ws) =
    WarningStorage <$>
        traverseKeys
            (\(FileBuffer path) -> do
                let path' = case maybeCustomMakeDir of
                        Just d -> d </> path
                        Nothing -> path
                FileBuffer <$> canonicalizePath path')
            ws

traverseKeys :: (Applicative f, Ord b) => (a -> f b) -> M.Map a v -> f (M.Map b v)
traverseKeys f m = fromSwappedList <$> traverse (traverse f) (toSwappedList m)
    where fromSwappedList = M.fromList . map swap
          toSwappedList = map swap . M.toList