{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO:
-- 
--  * fuzzy-open dialog for error list
--  * Somehow mark tabs with errors. Maybe with color or with a '!'.

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
    ) where

import Control.Applicative
import Control.Concurrent
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
            showErrorE

debug :: EditorM ()
debug = do
    ws :: WarningStorage <- getEditorDyn
    withCurrentBuffer $ insertN (R.fromString (show ws))

showErrorE :: EditorM ()
showErrorE = do
    maybeOverlayAtPoint <- withCurrentBuffer $ do
        p <- pointB
        overlays <- getOverlaysOfOwnerB "make"
        return (find (isPointInsideOverlay p) overlays)
    winWidth <- fmap width (use currentWindowA)
    case maybeOverlayAtPoint of
        Just overlay ->
            printMsgs
                (fmap
                    (T.justifyLeft (winWidth - 1) ' ' . R.toText)
                    (R.lines (overlayAnnotation overlay)))
        Nothing -> return ()

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
            case T.words (unMakePrg makeprg) of
                [] -> error "empty makeprg"
                ws -> fmap T.unpack ws
    printMsg ("Launching " <> unMakePrg makeprg)
    x <- ask
    void . io . forkIO $ do
        (code, out, err) <- readProcessWithExitCode cmd args ""
        ws@(WarningStorage warningsByBuffer) <-
            fixPathsInBufferIds (parseWarningStorage (out <> "\n" <> err))
        let action = do
                putEditorDyn ws
                bufs <- fmap M.toList (gets buffers)
                forM_ bufs $ \(ref, buf) ->
                    withGivenBuffer ref . retroactivelyAtSavePointB $ do
                        delOverlaysOfOwnerB "make"
                        case M.lookup (buf ^. identA) warningsByBuffer of
                            Just warnings ->
                                    V.mapM_
                                        (addOverlayB <=< messageToOverlayB)
                                        warnings
                            _ -> return ()
                case code of
                    ExitSuccess ->
                        printMsg (unMakePrg makeprg <> " finished successfully.")
                    _ -> printMsg "Make failed"
        yiOutput x MustRefresh [EditorA action]

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

messageToOverlayB :: Warning -> BufferM Overlay
messageToOverlayB (Warning _ l1 c1 l2 c2 msg) = savingPointB $ do
    moveToLineColB l1 (c1 - 1)
    p1 <- pointB
    if c2 >= 0
    then moveToLineColB l2 c2
    else moveToLineColB l2 0 >> moveToEol
    p2 <- pointB
    return (mkOverlay "make" (mkRegion p1 p2) errorStyle (R.fromText msg))

fixPathsInBufferIds :: WarningStorage -> IO WarningStorage
fixPathsInBufferIds (WarningStorage ws) =
    WarningStorage <$>
        traverseKeys
            (\(FileBuffer path) -> FileBuffer <$> canonicalizePath path)
            ws

traverseKeys :: (Applicative f, Ord b) => (a -> f b) -> M.Map a v -> f (M.Map b v)
traverseKeys f m = fromSwappedList <$> traverse (traverse f) (toSwappedList m)
    where fromSwappedList = M.fromList . map swap
          toSwappedList = map swap . M.toList