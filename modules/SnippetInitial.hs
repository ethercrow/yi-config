{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Snippet
    ( expandSnippet
    , Snippet (..)
    , SnipM
    , lit
    , finish
    , refer
    , place
    , line
    , nl
    , mirror
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad.State
import Data.Maybe (listToMaybe)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.String
import qualified Data.Text as T

import Yi
import qualified Yi.Rope
import qualified Yi.Rope as R

data Snippet = Snippet
    { snipTrigger :: !T.Text
    , snipBody :: SnipM ()
    }

data Var = Var Int
    deriving (Eq, Ord, Show)

data SnipF next
    = Write' R.YiString next
    | Finish' next
    | Place' R.YiString (Var -> next)
    | Refer' Var (R.YiString -> next)
    deriving Functor

data SnipM :: * -> * where
    Write :: R.YiString -> SnipM ()
    Place :: R.YiString -> SnipM Var
    Finish :: SnipM ()
    Refer :: Var -> SnipM R.YiString

    Return :: a -> SnipM a
    Bind :: SnipM a -> (a -> SnipM b) -> SnipM b

instance Functor SnipM where
    fmap f (Return x) = Return (f x)
    fmap f x = pure f <*> x

instance Applicative SnipM where
    pure = Return
    af <*> ax = do
        f <- af
        x <- ax
        return (f x)

instance Monad SnipM where
    return = Return
    (>>=) = Bind

line :: R.YiString -> SnipM ()
line s = Write s >> nl

nl :: SnipM ()
nl = Write "\n"

mirror :: Var -> SnipM ()
mirror = Write <=< Refer

lit = Write
place = Place
refer = Refer
finish = Finish

expandSnippet :: Snippet -> BufferM ()
expandSnippet (Snippet trigger body) = do
    let env = initialEnv body
    insertN (renderBody body env)

renderBody :: SnipM () -> M.Map Var R.YiString -> R.YiString
renderBody _ env = fromString (show env)

initialEnv :: SnipM () -> M.Map Var R.YiString
initialEnv body = M.fromList [(Var 0, "ohai")]

beginPlaceholderEditing _state = return ()

-- expandSnippet (Snippet _ body) = withCurrentBuffer $ do
--     let f = "snippetFinish"
--     marks <- forM body $ \case
--         Literal s -> insertN s
--         Finish -> pointB >>= newMarkB
--     case listToMaybe marks of
--         Just _ -> do
--             m <- getMarkB (Just f)
--             moveTo . markPoint =<< getMarkValueB m
--             deleteMarkB m
--             insertN ("Just " <> R.fromString (show m))
--         _ -> do
--             insertN "Nothing"
--             return ()