
module FuzzySnippet
    ( fuzzySnippet
    ) where

import Yi

import Fuzzy
import Snippet

fuzzySnippet :: [Snippet] -> YiM ()
fuzzySnippet = genericFuzzy . fmap toFuzzyItem

toFuzzyItem s@(Snippet trigger body) = FuzzyItem
    { fiToText = trigger
    , fiAction = withCurrentBuffer (expandSnippet s)
    }