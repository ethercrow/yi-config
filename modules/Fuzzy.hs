{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- Keymap:
--
--   <C-p> (or whatever mapping user chooses) starts fuzzy open dialog.
--
--   Typing something filters filelist.
--
--   <Enter> opens currently selected file
--   in current (the one that fuzzyOpen was initiated from) window.
--
--   <C-t> opens currently selected file in a new tab.
--   <C-s> opens currently selected file in a split.
--
--   <KUp> and <C-p> moves selection up
--   <KDown> and <C-n> moves selection down
--
--   Readline shortcuts <C-a> , <C-e>, <C-u> and <C-k> work as usual.
--
--   TODO if need arises: factor out generic part that captures a pattern of
--   having an interactive minibuffer and a window that just renders some state.

module Fuzzy
    ( FuzzyItem (..)
    , genericFuzzy
    ) where

import Control.Monad.ST
import Data.Binary
import Data.Default
import Data.Foldable (Foldable, toList)
import Data.Monoid
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as Introsort

import Yi
import Yi.MiniBuffer
import qualified Yi.Rope as R
import Yi.Types
import Yi.Utils ()

import FuzzyMatchScore

-- FuzzyState is stored in minibuffer's dynamic state
data FuzzyState = FuzzyState
    { _fsItems :: !(V.Vector FuzzyItem)
    , fsSelectedIndex :: !(Maybe Int)
    , fsNeedle :: !T.Text
    } deriving (Typeable)

data FuzzyItem = FuzzyItem
    { fiAction :: YiM ()
    , fiToText :: T.Text
    }

genericFuzzy :: Foldable f => f FuzzyItem -> YiM ()
genericFuzzy items = do
    promptRef <- withEditor (spawnMinibufferE "" (const localKeymap))
    let initialState =
            FuzzyState (V.fromList (toList items))
                       (Just 0)
                       ""
    withGivenBuffer promptRef $ do
        putBufferDyn initialState
    withEditor (renderE initialState)

localKeymap :: Keymap
localKeymap =
    choice
        [ spec KEnter ?>>! openInThisWindow
        , ctrlCh 't'  ?>>! openInNewTab
        , ctrlCh 's'  ?>>! openInSplit
        , spec KEsc   ?>>! cleanupE
        , ctrlCh 'g'  ?>>! cleanupE
        , ctrlCh 'h'  ?>>! updatingB (deleteB Character Backward)
        , spec KBS    ?>>! updatingB (deleteB Character Backward)
        , spec KDel   ?>>! updatingB (deleteB Character Backward)
        , ctrlCh 'a'  ?>>! moveToSol
        , ctrlCh 'e'  ?>>! moveToEol
        , spec KLeft  ?>>! moveXorSol 1
        , spec KRight ?>>! moveXorEol 1
        , ctrlCh 'p'  ?>>! modifyE decrementIndex
        , spec KUp    ?>>! modifyE decrementIndex
        , ctrlCh 'n'  ?>>! modifyE incrementIndex
        , spec KDown  ?>>! modifyE incrementIndex
        , ctrlCh 'w'  ?>>! updatingB (deleteB unitWord Backward)
        , ctrlCh 'u'  ?>>! updatingB (moveToSol >> deleteToEol)
        , ctrlCh 'k'  ?>>! updatingB deleteToEol
        ]
    <|| (insertChar >>! ((withCurrentBuffer updateNeedleB) >>= renderE))
    where updatingB :: BufferM () -> EditorM ()
          updatingB bufAction = withCurrentBuffer (bufAction >> updateNeedleB) >>= renderE

updateNeedleB :: BufferM FuzzyState
updateNeedleB = do
   needle <- R.toText <$> readLnB
   oldState <- getBufferDyn
   let intermediateState = oldState { fsNeedle = needle }
       newState = intermediateState
           { fsSelectedIndex =
               case V.toList (filteredItems intermediateState) of
                   [] -> Nothing
                   (_, index) : _ -> Just index
           }
   putBufferDyn newState
   return newState

filteredItems :: FuzzyState -> (V.Vector (FuzzyItem, Int))
filteredItems (FuzzyState items _ needle) =
    V.map
        (\(_score, item, index) -> (item, index))
        (sortByScore
            (V.filter
                ((> 0) . fst3)
                (V.zip3
                    scores
                    items
                    (V.enumFromTo 0 (V.length items)))))
    where
        fst3 (x, _, _) = x
        scores = V.fromList (fmap snd (scoreAll needle (V.toList (V.map fiToText items))))
        sortByScore v = runST $ do
            mv <- V.thaw v
            Introsort.sortBy (comparing (negate . fst3)) mv
            V.freeze mv

modifyE :: (FuzzyState -> FuzzyState) -> EditorM ()
modifyE f = do
    prevState <- withCurrentBuffer getBufferDyn
    let newState = f prevState
    withCurrentBuffer (putBufferDyn newState)
    renderE newState

incrementIndex :: FuzzyState -> FuzzyState
incrementIndex fs@(FuzzyState _ Nothing _) = fs
incrementIndex fs@(FuzzyState _ (Just index) _) =
    let fitems = filteredItems fs
        steps = V.zipWith (\x y -> (snd x, snd y)) fitems (V.tail fitems)
        newIndex =  case V.find ((== index) . fst) steps of
            Nothing -> Just index
            Just (_, nextIndex) -> Just nextIndex
    in fs { fsSelectedIndex = newIndex }

decrementIndex :: FuzzyState -> FuzzyState
decrementIndex fs@(FuzzyState _ Nothing _) = fs
decrementIndex fs@(FuzzyState _ (Just index) _) =
    let fitems = filteredItems fs
        steps = V.zipWith (\x y -> (snd x, snd y)) (V.tail fitems) fitems
        newIndex = case V.find ((== index) . fst) steps of
            Nothing -> Just index
            Just (_, prevIndex) -> Just prevIndex
    in fs { fsSelectedIndex = newIndex }

renderE :: FuzzyState -> EditorM ()
renderE fs@(FuzzyState _ selIndex _) = do
    let content = V.toList (fmap renderItem (filteredItems fs))
        -- TODO justify to actual screen width
        renderItem (item, itemIndex) = mconcat
            [ (if Just itemIndex == selIndex then "* " else "  ")
            , fiToText item
            ]
    setStatus (content, defaultStyle)

openInThisWindow :: YiM ()
openInThisWindow = openRoutine (return ())

openInSplit :: YiM ()
openInSplit = openRoutine splitE

openInNewTab :: YiM ()
openInNewTab = openRoutine newTabE

openRoutine :: EditorM () -> YiM ()
openRoutine preOpenAction = do
    FuzzyState items mselIndex _ <- withCurrentBuffer getBufferDyn
    case mselIndex of
        Nothing -> printMsg "Nothing selected"
        Just selIndex -> do
            withEditor $ do
                cleanupE
                preOpenAction
            fiAction (items V.! selIndex)

insertChar :: Keymap
insertChar = textChar >>= write . insertB

cleanupE :: EditorM ()
cleanupE = clrStatus >> closeBufferAndWindowE

-- Let's survive without reloading editor in the middle of fuzzy open for now
instance Binary FuzzyState where
    put (FuzzyState _items _index _needle) = return ()
    get = return def

instance Default FuzzyState where
    def = FuzzyState mempty Nothing mempty

instance YiVariable FuzzyState
