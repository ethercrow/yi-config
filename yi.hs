{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.State
import qualified Data.Text as T
import           Yi hiding (super)
import qualified Yi.Keymap.Vim as V
import qualified Yi.Keymap.Vim.Common as V
import qualified Yi.Keymap.Vim.Utils as V
import qualified Yi.Mode.Haskell as Haskell
import qualified Yi.Rope as R

import           Fuzzy

main :: IO ()
main = yi $ defaultVimConfig {
    modeTable = fmap prefIndent (modeTable defaultVimConfig),
    defaultKm = myKeymapSet,
    configCheckExternalChangesObsessively = False,
    startActions = [EditorA (do
        e <- get
        put e { maxStatusHeight = 30 })]
}

myKeymapSet :: KeymapSet
myKeymapSet = V.mkKeymapSet $ V.defVimConfig `override` \super this ->
    let eval = V.pureEval this
    in super
        { V.vimBindings = myBindings eval ++ V.vimBindings super
        , V.vimRelayout = colemakRelayout
        }

myBindings :: (V.EventString -> EditorM ()) -> [V.VimBinding]
myBindings eval =
    let nmap x y = V.mkStringBindingE V.Normal V.Drop (x, y, id)
        nmapY x y = V.mkStringBindingY V.Normal (x, y, id)
        imap x y = V.VimBindingE (\evs state -> case V.vsMode state of
                                    V.Insert _ ->
                                        fmap (const (y >> return V.Continue))
                                             (evs `V.matchesString` x)
                                    _ -> V.NoMatch)
    in [ nmap "<BS>" previousTabE
       , nmap "<Tab>" nextTabE
       , nmap " " (eval ":nohlsearch<CR>")
       , nmap ";" (eval ":")
       , nmapY "<C-;>" fuzzyOpen
       ]

colemakRelayout :: Char -> Char
colemakRelayout = V.relayoutFromTo colemakLayout qwertyLayout
    where
        colemakLayout = concat ["qwfpgjluy;[]", "arstdhneio'\\", "zxcvbkm,./"]
        qwertyLayout = concat ["qwertyuiop[]", "asdfghjkl;'\\", "zxcvbnm,./"]

prefIndent :: AnyMode -> AnyMode
prefIndent = onMode $ \m -> m {
    modeIndentSettings = IndentSettings
        { expandTabs = True
        , shiftWidth = 4
        , tabSize = 4
        }}
