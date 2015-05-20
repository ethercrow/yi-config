#!/usr/bin/env runhaskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map.Strict as M
import Data.Monoid
import Test.Tasty.TH
import Test.Tasty.HUnit

import qualified Yi.Rope as R

import Snippet

main :: IO ()
main = $defaultMainGenerator

lp :: Snippet
lp = Snippet "lp" $ do
  lit "{-# LANGUAGE "
  _ <- place "OverloadedStrings"
  line " #-}"

useSnippet :: Snippet -> [EditAction] -> R.YiString
useSnippet snip actions =
    renderSnippet snip (foldl advanceEditState (initialEditState snip) actions)

case_lp_default :: Assertion
case_lp_default =
    useSnippet lp [] @?= "{-# LANGUAGE OverloadedStrings #-}\n"

case_lp_custom :: Assertion
case_lp_custom =
    useSnippet lp (map SEInsertChar "LambdaCase")
        @?= "{-# LANGUAGE LambdaCase #-}\n"

case_lp_backspace :: Assertion
case_lp_backspace =
    useSnippet lp (map SEInsertChar "LambdaCasx" <> [SEBackSpace] <> [SEInsertChar 'e'])
        @?= "{-# LANGUAGE LambdaCase #-}\n"

case_collect_vars :: Assertion
case_collect_vars =
    let Snippet _ body = lp
    in collectVars body @?= M.fromList [(0, DefaultValue "OverloadedStrings")]
    