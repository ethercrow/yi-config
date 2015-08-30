{-# LANGUAGE OverloadedStrings #-}

module LuaMode
    ( luaMode
    ) where

import Control.Lens
import Data.Loc
import Data.Maybe
import Data.Data.Lens
import Language.Lua.Lexer
import Language.Lua.Parser
import Language.Lua.Syntax
import Language.Lua.Token

import Yi.Buffer.Misc
import Yi.Modes
import Yi.Style
import Yi.Syntax

type LuaStorage = Maybe (Chunk NodeInfo)

luaMode :: Mode LuaStorage
luaMode = fundamentalMode
    { modeName = "lua"
    , modeApplies = anyExtension ["lua"]
    , modeHL = ExtHL luaHighlighter
    , modeGetStrokes = luaGetStrokes
    }

luaHighlighter = SynHL
    { hlStartState = Nothing
    , hlRun = \scanner _point _oldTokens ->
        let contentString = fmap snd (scanRun scanner (scanInit scanner))
            tokens = streamToList (runLexer luaLexer "" contentString)
        in case fullParses (parser luaChunk tokens) of
            ([x], _) -> Just x
            _ -> Nothing
    , hlGetTree = \tokens _windowRef -> tokens
    , hlFocus = \_refToRegion tokens -> tokens
    }

luaGetStrokes :: LuaStorage -> Point -> Point -> Point -> [Stroke]
luaGetStrokes mchunk _point _begin _end =
    mapMaybe expToStroke (toListOf tinplate mchunk)

expToStroke (String info _) = Just (mkStroke info stringStyle)
expToStroke _ = Nothing

mkStroke (NodeInfo (Loc (Pos _ _ _ begin) (Pos _ _ _ end)) _) sty =
    Span (Point begin) sty (Point (end + 1))