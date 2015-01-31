-- Copyright (c) 2014 Curtis Gagliardi

-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:

-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

{-# LANGUAGE ViewPatterns #-}

module FuzzyMatchScore where

import           Control.Parallel.Strategies
import qualified Data.Text                   as T

-- TODO: add tests
minMatchLength :: T.Text -> T.Text -> Int
minMatchLength (T.uncons -> Nothing) _  =  1
minMatchLength _ (T.uncons -> Nothing)  =  0
minMatchLength (T.uncons -> Just (qHead, rest)) choice =
    let matchLengths =  filter (>0)
                        . map (\t -> endMatch rest (T.drop 1 t) 1)
                        . filter ((== qHead) . T.head)
                        . filter (not . T.null)
                        $ T.tails choice
    in  if null matchLengths
        then 0
        else minimum matchLengths
  where
    endMatch :: T.Text -> T.Text -> Int -> Int
    endMatch (T.uncons -> Nothing) _ lastIndex = lastIndex
    endMatch (T.uncons -> Just (q, qs)) s lastIndex =
        case T.findIndex (== q) s of
            Just i -> endMatch qs (T.drop (i + 1) s) (i + 1 + lastIndex)
            Nothing -> 0

normalizeScore :: Int -> T.Text -> T.Text -> Double
normalizeScore matchLength query choice
    | matchLength <= 0 = 0
    | otherwise =
          fromIntegral (T.length query)
          / fromIntegral matchLength       -- penalize longer match lengths
          / fromIntegral (T.length choice) -- penalize longer choice strings

score :: T.Text -> T.Text -> Double
score q choice
    | T.null q      = 1
    | T.null choice = 0
    | otherwise = let minLength = minMatchLength q (T.toLower choice)
                  in normalizeScore minLength q choice

scoreAll :: T.Text -> [T.Text] -> [(T.Text, Double)]
scoreAll query choices =
    map (\choice -> (choice, score (T.toLower query) choice)) choices
       `using` parListChunk 1000 rdeepseq
