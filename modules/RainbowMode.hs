{-# LANGUAGE OverloadedStrings #-}

module RainbowMode
    ( rainbowParenMode
    ) where

import Data.Foldable (asum)
import Data.Maybe

import Yi hiding (super)
import Yi.Mode.Common (fundamentalMode)
import Yi.Modes
import Yi.Lexer.Alex
import Yi.Syntax

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

rainbowParenMode :: Mode RainbowStorage
rainbowParenMode = fundamentalMode
    { modeName = "rainbow"
    , modeHL = ExtHL rainbowHighlighter
    , modeGetStrokes = rainbowGetStrokes
    }

type Level = Int
type RainbowStorage = [(Paren, Level)]

rainbowHighlighter :: Highlighter RainbowStorage RainbowStorage
rainbowHighlighter = SynHL
    []
    (\scanner _point _oldTokens ->
        fromMaybe
            []
            (tokenize
                (scanRun scanner (scanInit scanner))))
    (\tokens _windowRef -> tokens)
    (\_refToRegion tokens -> tokens)

rainbowGetStrokes :: RainbowStorage -> Point -> Point -> Point -> [Stroke]
rainbowGetStrokes tokens _point _begin _end =
    map
        (\(token, level) ->
            let pos = tokenPoint token
            in Span pos (styleForLevel level) (pos + 1))
        tokens

tokenize :: [(Point, Char)] -> Maybe [(Paren, Int)]
tokenize input = fmap assignLevels (input =~ lexTokens)

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