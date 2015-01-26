{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -imodules #-}

import Control.Lens hiding (imap)
import Control.Monad.State hiding (state)

import Yi hiding (super)
import qualified Yi.Keymap.Vim as V
import qualified Yi.Keymap.Vim.Common as V
import qualified Yi.Keymap.Vim.Utils as V

import Fuzzy
import Make
import RainbowMode

main :: IO ()
main = startEditor myConfig Nothing

myConfig :: Config
myConfig = defaultVimConfig
    { modeTable = fmap prefIndent (myModes defaultVimConfig)
    , defaultKm = myKeymapSet
    , configCheckExternalChangesObsessively = False
    , startActions = [EditorA (do
        e <- get
        put e { maxStatusHeight = 30 })]
    }

myKeymapSet :: KeymapSet
myKeymapSet = V.mkKeymapSet $ V.defVimConfig `override` \super this ->
    let eval = V.pureEval this
    in super
        { V.vimBindings = myBindings eval ++ V.vimBindings super
        , V.vimRelayout = colemakRelayout
        , V.vimExCommandParsers = exMake : exMakePrg : V.vimExCommandParsers super
        }

myBindings :: (V.EventString -> EditorM ()) -> [V.VimBinding]
myBindings eval =
    let nmap x y = V.mkStringBindingE V.Normal V.Drop (x, y, id)
        nmapY x y = V.mkStringBindingY V.Normal (x, y, id)
        _imap x y = V.VimBindingE (\evs state -> case V.vsMode state of
                                    V.Insert _ ->
                                        fmap (const (y >> return V.Continue))
                                             (evs `V.matchesString` x)
                                    _ -> V.NoMatch)
    in [ nmap "<BS>" previousTabE
       , nmap "<Tab>" nextTabE
       , nmap " " (eval ":nohlsearch<CR>")
       , nmap ";" (eval ":")
       , nmapY "<C-;>" fuzzyOpen
       , nmap "<M-l>" (withCurrentBuffer (transposeB unitWord Forward >> leftB))
       , nmap "<M-h>" (withCurrentBuffer (transposeB unitWord Backward))
       , nmap "<C-@>" showErrorE
       , nmap "<M-d>" debug
       ]

colemakRelayout :: Char -> Char
colemakRelayout = V.relayoutFromTo colemakLayout qwertyLayout
    where
        colemakLayout = concat ["qwfpgjluy;[]", "arstdhneio'\\", "zxcvbkm,./"]
        qwertyLayout = concat ["qwertyuiop[]", "asdfghjkl;'\\", "zxcvbnm,./"]

prefIndent :: AnyMode -> AnyMode
prefIndent = onMode $ \m ->
    if m ^. modeNameA == "Makefile"
    then m
    else m
        { modeIndentSettings = IndentSettings
            { expandTabs = True
            , shiftWidth = 4
            , tabSize = 4
            }}

myModes :: Config -> [AnyMode]
myModes cfg = AnyMode rainbowParenMode : modeTable cfg