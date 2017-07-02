{-# LANGUAGE OverloadedStrings #-}

module RainbowMode
    ( rainbowParenMode
    ) where

-- TODO
-- * highlight paren paired to the one under cursor

import Data.Foldable (asum)
import Data.Maybe

import Yi hiding (super)
import Yi.Mode.Common (fundamentalMode)

import Text.Regex.Applicative

data Paren
    = Open !Point
    | Close !Point

styleForLevel :: Level -> StyleName
styleForLevel = (colors !!) . (`rem` length colors)
    where
        colors =
            [ importStyle
            , stringStyle
            , numberStyle
            , dataConstructorStyle
            ]

tokenPoint :: Paren -> Point
tokenPoint (Open p) = p
tokenPoint (Close p) = p

rainbowParenMode :: Mode
rainbowParenMode = fundamentalMode
    { modeName = "rainbow"
    , modeGetStrokes = rainbowGetStrokes
    }

type Level = Int
type RainbowStorage = [(Paren, Level)]

rainbowGetStrokes :: Point -> Point -> Point -> BufferM [Stroke]
rainbowGetStrokes _point _begin _end =
    map toSpan . tokenize <$> indexedStreamB Forward (Point 0)
    where
    toSpan (token, level) =
        let pos = tokenPoint token
        in Span pos (styleForLevel level) (pos + 1)

tokenize :: [(Point, Char)] -> RainbowStorage
tokenize input = fromMaybe [] $ fmap assignLevels (input =~ lexTokens)

lexTokens :: RE (Point, Char) [Paren]
lexTokens =
    fmap
        catMaybes
        (many
            (asum
                [ Just <$> lexToken
                , Nothing <$ lexIrrelevant
                ]))

lexToken :: RE (Point, Char) Paren
lexToken = asum
    [ Open . fst <$> psym ((== '(') . snd)
    , Close . fst <$> psym ((== ')') . snd)
    ]

lexIrrelevant :: RE (Point, Char) ()
lexIrrelevant = () <$ many (psym ((`notElem` ['(', ')']) . snd))

assignLevels :: [Paren] -> [(Paren, Int)]
assignLevels = go [] 0
    where
        go acc _ [] = reverse acc
        go acc 0 (Close p : tokens) = go ((Close p, 0) : acc) 0 tokens
        go acc level (t@(Open _) : tokens) =
            go ((t, level) : acc) (level + 1) tokens
        go acc level (t@(Close _) : tokens) =
            go ((t, level - 1) : acc) (level - 1) tokens