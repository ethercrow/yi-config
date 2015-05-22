{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Snippet.Internal
    ( Snippet (..)
    , Var
    , VarValue (..)
    , SnippetBody
    , EditState (..)
    , EditAction (..)
    , initialEditState
    , lit
    , line
    , nl
    , place
    , refer
    , finish
    , mirror
    , renderSnippet
    , collectVars
    , advanceEditState
    , expandSnippetB
    ) where

import Control.Monad.Free
import Control.Monad.State hiding (state)
import Control.Monad.Writer
import qualified Data.Map.Strict as M
import Data.Maybe

import Yi.Buffer
import qualified Yi.Rope as R

data Snippet = Snippet
    { snipTrigger :: R.YiString
    , snipBody :: SnippetBody ()
    }

type Var = Int
data VarValue
    = DefaultValue R.YiString
    | CustomValue R.YiString
    deriving (Show, Eq)
type Vars = M.Map Var VarValue

data SnippetBodyF a
    = Lit R.YiString a
    | Finish a
    | MakeVar R.YiString (Var -> a)
    | Mirror Var a
    | Refer Var (R.YiString -> a)
    deriving Functor

type SnippetBody = Free SnippetBodyF

lit :: R.YiString -> SnippetBody ()
lit s = liftF (Lit s ())

line :: R.YiString -> SnippetBody ()
line s = lit (s <> "\n")

nl :: SnippetBody ()
nl = liftF (Lit "\n" ())

finish :: SnippetBody ()
finish = liftF (Finish ())

place :: R.YiString -> SnippetBody Var
place s = do
    var <- liftF (MakeVar s id)
    mirror var
    return var

refer :: Var -> SnippetBody R.YiString
refer var = liftF (Refer var id)

mirror :: Var -> SnippetBody ()
mirror var = liftF (Mirror var ())

data EditState = EditState
    { sesCursorPosition :: (Maybe Var, Int)
    , sesVars :: Vars
    } deriving (Show, Eq)

initialEditState :: Snippet -> EditState
initialEditState (Snippet _ body) =
    EditState
        (listToMaybe (M.keys vars), 0)
        vars
    where
    vars = collectVars body

collectVars :: SnippetBody a -> Vars
collectVars body =
    snd (runState (iterM run body) mempty)
    where
    run :: SnippetBodyF (State Vars a) -> State Vars a
    run (Lit _ rest) = rest
    run (Finish rest) = rest
    run (MakeVar s f) = do
        vars <- get
        let newVar = if M.null vars then 0 else maximum (M.keys vars) + 1
            newVars = M.insert newVar (DefaultValue s) vars
        put newVars
        f newVar
    run (Mirror _ rest) = rest
    run (Refer var f) = do
        vars <- get
        f (toYiString (vars M.! var))

data EditAction
    = SENext
    | SEInsertChar Char
    | SEBackSpace
    | SEEscape

renderSnippet :: Snippet -> EditState -> (Int, R.YiString)
renderSnippet (Snippet _ body) (EditState (maybeActiveVar, offset) vars) = 
    (either id id epos, string)
    where
    (((), (_var, epos)), string) = runWriter (runStateT (iterM run body) (-1, Right 0))
    advance :: MonadState (Var, Either Int Int) m => Int -> m ()
    advance n = modify (fmap (fmap (+ n)))
    run :: SnippetBodyF ((StateT (Var, Either Int Int) (Writer R.YiString)) a)
        -> StateT (Var, Either Int Int) (Writer R.YiString) a
    run (Lit s rest) = do
        tell s
        advance (R.length s)
        rest
    run (Finish rest) = rest
    run (Mirror var rest) = do
        let s = toYiString (vars M.! var)
        tell s

        if Just var == maybeActiveVar
        then do
            (v, curPos) <- get
            case curPos of
                Right pos ->
                    put (v, (Left (pos + offset)))
                _ -> return ()
        else advance (R.length s)

        rest
    run (MakeVar _ f) = do
        (varName, pos) <- get
        put (varName + 1, pos)
        f (varName + 1)
    run (Refer var f) = f (toYiString (vars M.! var))

toYiString :: VarValue -> R.YiString
toYiString (DefaultValue s) = s
toYiString (CustomValue s) = s

advanceEditState :: EditState -> EditAction -> EditState
advanceEditState state@(EditState (Nothing, _) _) SENext = state
advanceEditState (EditState (Just i, pos) vars) (SEInsertChar c) =
    let newVars = M.adjust (insertChar c pos) i vars
    in EditState (Just i, pos + 1) newVars
advanceEditState (EditState (Just i, pos) vars) SEBackSpace =
    let newVars = M.adjust (backspace pos) i vars
    in EditState (Just i, pos - 1) newVars
advanceEditState (EditState (Just i, _) vars) SENext =
    let nextPlace = listToMaybe (dropWhile (<= i) (M.keys vars))
    in EditState (nextPlace, 0) vars
advanceEditState state _ = state

insertChar :: Char -> Int -> VarValue -> VarValue
insertChar c _ (DefaultValue _) = CustomValue (R.singleton c)
insertChar c pos (CustomValue s) = CustomValue (lhs <> R.singleton c <> rhs)
    where (lhs, rhs) = R.splitAt pos s

backspace :: Int -> VarValue -> VarValue
backspace _ (DefaultValue _) = CustomValue mempty
backspace 0 v = v
backspace pos (CustomValue s) = CustomValue (lhs <> R.drop 1 rhs)
    where (lhs, rhs) = R.splitAt (pos - 1) s

expandSnippetB :: [Snippet] -> BufferM Bool
expandSnippetB snippets = do
    trigger <- readPrevWordB
    let match = listToMaybe (filter ((== trigger) . snipTrigger) snippets)
    case match of
        Just snip -> do
            deleteB unitWord Backward
            Point origin <- pointB
            let (offset, s) = renderSnippet snip (initialEditState snip)
            insertN s
            moveTo (Point (origin + offset))
            return True
        _ -> return False