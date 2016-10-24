{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -imodules #-}

import Control.Monad.State hiding (state)
import Data.List (intersperse)
import Data.Monoid
import qualified Data.Text as T
import Lens.Micro.Platform
import System.Directory (getCurrentDirectory)
import System.Environment

import Yi hiding (super)
import Yi.Utils (io)
import Yi.Modes (gnuMakeMode)
import qualified Yi.Keymap.Vim as V
import qualified Yi.Keymap.Vim.Common as V
import qualified Yi.Keymap.Vim.Ex.Types as V
import qualified Yi.Keymap.Vim.Ex.Commands.Common as V
import qualified Yi.Keymap.Vim.Utils as V
import qualified Yi.Frontend.Vty as Vty

import FuzzyFile
import Make
import qualified Snippet
import MySnippets

import RainbowMode
import PyflakesMode

main :: IO ()
main = do
    files <- getArgs
    let actions = intersperse (EditorA newTabE) (map (YiA . openNewFile) files)
    startEditor (myConfig actions) Nothing

myConfig :: [Action] -> Config
myConfig actions = defaultConfig
    { modeTable =
        fmap
            (configureModeline . configureIndent)
            (myModes defaultConfig)
    , startFrontEnd = Vty.start
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
            exMake : exFlakes : exMakePrgOption : exPwd :
                V.vimExCommandParsers super
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
        defEval = V.pureEval (extractValue V.defVimConfig)
    in [ nmap "<BS>" previousTabE
       , nmap "<Tab>" nextTabE
       , nmap " " (eval ":nohlsearch<CR>")
       , nmap ";" (eval ":")
       , nmapY "<C-;>" fuzzyFile
       , nmap "<M-l>" (withCurrentBuffer (transposeB unitWord Forward >> leftB))
       , nmap "<M-h>" (withCurrentBuffer (transposeB unitWord Backward))
       , nmap "<C-@>" showErrorE
       , nmap "<M-d>" debug
       , nmapY "s" (jumpToNextErrorInCurrentBufferY Forward)
       , nmapY "S" (jumpToNextErrorY Forward)
       , nmap ",s" insertErrorMessageE
       , imapY "<Tab>"
           (withEditor $ do
               expanded <- Snippet.expandSnippetE (defEval "<Esc>") mySnippets 
               when (not expanded) (defEval "<Tab>"))
       , nmapY "<Esc>" (flakes >> withEditor (defEval "<Esc>"))
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
    : AnyMode pyflakesMode
    : AnyMode rainbowParenMode
    : modeTable cfg

exPwd :: V.EventString -> Maybe V.ExCommand
exPwd "pwd" = Just (V.impureExCommand{V.cmdAction = YiA (io getCurrentDirectory >>= printMsg . T.pack)})
exPwd _ = Nothing

showT :: Show a => a -> T.Text
showT = T.pack . show