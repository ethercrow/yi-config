{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -imodules #-}

import Control.Lens hiding (argument, imap)
import Control.Monad.State hiding (state)
import Data.List (intersperse)
import Data.Monoid
import qualified Data.Text as T
import System.Console.Docopt
import System.Environment

import Yi hiding (super)
import Yi.Modes (gnuMakeMode)
import qualified Yi.Keymap.Vim as V
import qualified Yi.Keymap.Vim.Common as V
import qualified Yi.Keymap.Vim.Utils as V

import FuzzyFile
import Make
import qualified Snippet
import MySnippets
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
    { modeTable =
        fmap
            (configureModeline . configureIndent)
            (myModes defaultVimConfig)
    , defaultKm = myKeymapSet
    , configCheckExternalChangesObsessively = False
    , startActions =
        (EditorA (do
            e <- get
            put e { maxStatusHeight = 30 }))
        : YiA guessMakePrg
        : actions
    }

myKeymapSet :: KeymapSet
myKeymapSet = V.mkKeymapSet $ V.defVimConfig `override` \super this ->
    let eval = V.pureEval this
    in super
        { V.vimBindings = myBindings eval ++ V.vimBindings super
        , V.vimRelayout = colemakRelayout
        , V.vimExCommandParsers =
            exMake : exMakePrgOption : V.vimExCommandParsers super
        }

myBindings :: (V.EventString -> EditorM ()) -> [V.VimBinding]
myBindings eval =
    let nmap x y = V.mkStringBindingE V.Normal V.Drop (x, y, id)
        nmapY x y = V.mkStringBindingY V.Normal (x, y, id)
        imapY x y = V.VimBindingY (\evs state -> case V.vsMode state of
                                    V.Insert _ ->
                                        fmap (const (y >> return V.Drop))
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
       , nmap "s" (jumpToNextErrorE Forward)
       , imapY "<C-f>"
           (withCurrentBuffer (Snippet.expandSnippetB mySnippets))

       -- , imapY "<C-j>" (fuzzySnippet =<< mySnippets)
       ]

colemakRelayout :: Char -> Char
colemakRelayout = V.relayoutFromTo colemakLayout qwertyLayout
    where
        colemakLayout = concat ["qwfpgjluy;[]", "arstdhneio'\\", "zxcvbkm,./"]
        qwertyLayout = concat ["qwertyuiop[]", "asdfghjkl;'\\", "zxcvbnm,./"]

configureIndent :: AnyMode -> AnyMode
configureIndent = onMode $ \m ->
    if m ^. modeNameA == "Makefile"
    then m
    else m
        { modeIndentSettings = IndentSettings
            { expandTabs = True
            , shiftWidth = 4
            , tabSize = 4
            }}

configureModeline :: AnyMode -> AnyMode
configureModeline = onMode $ \m -> m {modeModeLine = myModeLine}
    where
    myModeLine prefix = do
        (line, col) <- getLineAndCol
        ro <- use readOnlyA
        mode <- gets (withMode0 modeName)
        unchanged <- gets isUnchangedBuffer
        filename <- gets (shortIdentString (length prefix))
        errorCount <- errorCountB
        return $ T.unwords
            [ if ro then "RO" else ""
            , if unchanged then "--" else "**"
            , filename
            , "L", showT line
            , "C", showT col
            , mode
            , if errorCount > 0
                then (showT errorCount <> " errors")
                else ""
            ]

myModes :: Config -> [AnyMode]
myModes cfg
    = AnyMode gnuMakeMode
    : AnyMode rainbowParenMode
    : modeTable cfg

showT :: Show a => a -> T.Text
showT = T.pack . show