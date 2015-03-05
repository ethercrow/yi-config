{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -imodules #-}

import Control.Lens hiding (Action, argument, imap)
import Control.Monad.State hiding (state)
import Data.List (intersperse)
import System.Console.Docopt
import System.Environment

import Yi hiding (super)
import Yi.Modes (gnuMakeMode)
import qualified Yi.Keymap.Vim as V
import qualified Yi.Keymap.Vim.Common as V
import qualified Yi.Keymap.Vim.Utils as V

import FuzzyFile
import Make
import RainbowMode

help :: Docopt
help = [docopt|
    Usage:
      e [<file> ...]
      e (-h|--help)

    Options:
      -h,--help      Show usage
|]

main :: IO ()
main = do
    args <- parseArgsOrExit help =<< getArgs
    let files = getAllArgs args (argument "file")
        actions = intersperse (EditorA newTabE) (map (YiA . openNewFile) files)
    startEditor (myConfig actions) Nothing

myConfig :: [Action] -> Config
myConfig actions = defaultVimConfig
    { modeTable = fmap prefIndent (myModes defaultVimConfig)
    , defaultKm = myKeymapSet
    , configCheckExternalChangesObsessively = False
    , startActions =
        (EditorA (do
            e <- get
            put e { maxStatusHeight = 30 }))
        : actions
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
       , nmapY "<C-;>" fuzzyFile
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
myModes cfg
    = AnyMode gnuMakeMode
    : AnyMode rainbowParenMode
    : modeTable cfg
