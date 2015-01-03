{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Make where

import Control.Applicative
import Control.Concurrent
import Control.Lens hiding (Action)
import Control.Monad
import Control.Monad.State (gets)
import Control.Monad.Reader
import Data.Binary
import Data.Default
import Data.Foldable (Foldable, toList, find)
import Data.Maybe (mapMaybe)
import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Tuple (swap)
import Data.Typeable
import qualified Data.Vector as V
import Data.Vector.Binary ()
import GHC.Generics
import System.Directory
import System.Exit
import System.Process

import Yi
import qualified Yi.Rope as R
import Yi.Types
import Yi.Utils
import qualified Yi.Keymap.Vim.Common as V
import qualified Yi.Keymap.Vim.Ex.Types as V
import qualified Yi.Keymap.Vim.Ex.Commands.Common as V

import Warning

showErrorE :: EditorM ()
showErrorE = do
    maybeOverlayAtPoint <- withCurrentBuffer $ do
        p <- pointB
        overlays <- getOverlaysOfOwnerB "make"
        return (find (isPointInsideOverlay p) overlays)
    case maybeOverlayAtPoint of
        Just overlay -> printMsg (R.toText (overlayAnnotation overlay))
        Nothing -> return ()

exMake :: V.EventString -> Maybe V.ExCommand
exMake "make" = Just (V.impureExCommand{V.cmdAction = YiA make})
exMake "wm" = Just (V.impureExCommand{V.cmdAction = YiA (viWrite >> make)})
exMake _ = Nothing

exMakePrg :: V.EventString -> Maybe V.ExCommand
exMakePrg = V.parseTextOption "makeprg" $ \case
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

newtype Warnings = Warnings (M.Map BufferId (V.Vector Warning))
    deriving (Generic, Typeable, Show)

instance Default Warnings where
    def = Warnings def

instance Binary Warnings

instance YiVariable Warnings

make :: YiM ()
make = do
    makeprg <- withEditor getEditorDyn
    let cmd : args =
            case T.words (unMakePrg makeprg) of
                [] -> error "empty makeprg"
                ws -> fmap T.unpack ws
    printMsg "Launching make process"
    x <- ask
    void . io . forkIO $ do
        (code, out, err) <- readProcessWithExitCode cmd args ""
        ws@(Warnings warningsByBuffer) <-
            fixPathsInBufferIds (parseWarnings (out <> "\n" <> err))
        writeFile "warnings" (show ws)
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
                    ExitSuccess -> printMsg "Make finished successfully."
                    _ -> printMsg "Make failed"
        yiOutput x MustRefresh [EditorA action]

parseWarnings :: String -> Warnings
parseWarnings =
    Warnings . M.mapKeysMonotonic FileBuffer .
        mapFromValues cmFilePath . mapMaybe parseWarning . lines

mapFromValues :: (Foldable f, Applicative t, Monoid (t v), Ord k)
    => (v -> k) -> f v -> M.Map k (t v)
mapFromValues keyFun values =
    M.fromListWith (<>) (fmap (\v -> (keyFun v, pure v)) (toList values))

messageToOverlayB :: Warning -> BufferM Overlay
messageToOverlayB (Warning _ l1 c1 l2 c2) = savingPointB $ do
    moveToLineColB l1 (c1 - 1)
    p1 <- pointB
    if c2 >= 0
    then moveToLineColB l2 c2
    else moveToLineColB l2 0 >> moveToEol
    p2 <- pointB
    return (mkOverlay "make" (mkRegion p1 p2) errorStyle "ohai")

fixPathsInBufferIds :: Warnings -> IO Warnings
fixPathsInBufferIds (Warnings ws) =
    Warnings <$>
        traverseKeys
            (\(FileBuffer path) -> FileBuffer <$> canonicalizePath path)
            ws

traverseKeys :: (Applicative f, Ord b) => (a -> f b) -> M.Map a v -> f (M.Map b v)
traverseKeys f m = fromSwappedList <$> traverse (traverse f) (toSwappedList m)
    where fromSwappedList = M.fromList . map swap
          toSwappedList = map swap . M.toList