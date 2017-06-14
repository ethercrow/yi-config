{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module YiWarning
    ( WarningStorage (..)
    , messageToOverlayB
    , fixPathsInBufferIds
    , parseWarningStorage
    ) where

import Control.Exception
import Data.Binary
import Data.Default
import Data.Foldable (toList)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Tuple (swap)
import Data.Typeable
import qualified Data.Vector as V
import System.Directory
import System.FilePath

import Yi
import Yi.Types
import qualified Yi.Rope as R

import Text.Warning

messageToOverlayB :: (UIStyle -> Style) -> Warning -> BufferM Overlay
messageToOverlayB style (Warning _ l1 c1 l2 c2 msg) = savingPointB $ do
    moveToLineColB l1 (c1 - 1)
    p1 <- pointB
    if c2 >= 0
    then moveToLineColB l2 c2
    else moveToLineColB l2 0 >> moveToEol
    p2 <- pointB
    return (mkOverlay "make" (mkRegion p1 p2) style (R.fromText msg))

newtype WarningStorage = WarningStorage (M.Map BufferId (V.Vector Warning))
    deriving (Typeable, Show)

instance Default WarningStorage where
    def = WarningStorage def

instance Binary WarningStorage where
    get = fmap (WarningStorage . fmap V.fromList) get
    put (WarningStorage ws) = put (fmap V.toList ws)

instance YiVariable WarningStorage

fixPathsInBufferIds :: Maybe FilePath -> WarningStorage -> IO WarningStorage
fixPathsInBufferIds maybeCustomMakeDir (WarningStorage ws) =
    WarningStorage <$>
        traverseKeys
            (\(FileBuffer path) -> do
                let path' = case maybeCustomMakeDir of
                        Just d -> d </> path
                        Nothing -> path
                try (canonicalizePath path') >>= \case
                    Right canonPath -> pure (FileBuffer canonPath)
                    Left (exc :: SomeException) -> pure (FileBuffer path'))
            ws

traverseKeys :: (Applicative f, Ord b) => (a -> f b) -> M.Map a v -> f (M.Map b v)
traverseKeys f m = fromSwappedList <$> traverse (traverse f) (toSwappedList m)
    where fromSwappedList = M.fromList . map swap
          toSwappedList = map swap . M.toList

parseWarningStorage :: String -> WarningStorage
parseWarningStorage =
    WarningStorage . M.mapKeysMonotonic FileBuffer .
        mapFromValues cmFilePath . parseWarnings

mapFromValues :: (Foldable f, Applicative t, Monoid (t v), Ord k)
    => (v -> k) -> f v -> M.Map k (t v)
mapFromValues keyFun values =
    M.fromListWith (<>) (fmap (\v -> (keyFun v, pure v)) (toList values))