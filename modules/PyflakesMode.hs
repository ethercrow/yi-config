{-# language OverloadedStrings #-}
{-# language LambdaCase #-}

module PyflakesMode
    ( pyflakesMode
    , exFlakes
    , flakes
    ) where

import Control.Exception
import Control.Monad
import Control.Monad.State (gets)
import Data.Foldable
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Exit
import System.Process

import Yi hiding (super)
import Yi.Utils (io)
import Yi.Mode.Common (fundamentalMode)
import Yi.Modes
import qualified Yi.Rope as R
import qualified Yi.Keymap.Vim.Common as Vim
import qualified Yi.Keymap.Vim.Ex.Types as Vim
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Vim

import YiWarning

pyflakesMode :: Mode ()
pyflakesMode = fundamentalMode
    { modeName = "pyflakes"
    , modeApplies = anyExtension ["py"]
    }

exFlakes :: Vim.EventString -> Maybe Vim.ExCommand
exFlakes "flakes" = Just (Vim.impureExCommand{Vim.cmdAction = YiA flakes})
exFlakes _ = Nothing

flakes :: YiM ()
flakes =
    withCurrentBuffer (gets (withMode0 modeName)) >>= \case
        "pyflakes" -> flakes'
        _ -> return ()

flakes' :: YiM ()
flakes' = do
    text <- withCurrentBuffer elemsB
    possiblyException <- io . try $ do
        (code, out, err) <-
            readCreateProcessWithExitCode (shell "pyflakes") (R.toString text)
        let combinedOut = out <> "\n" <> err
            ws = parseWarningStorage combinedOut
        return (code, ws)
    case possiblyException of
        Left e -> printMsg (T.pack (show (e :: SomeException)))
        Right (code, ws@(WarningStorage warningsByBuffer)) -> withEditor $ do
            let style = if code == ExitSuccess then hintStyle else errorStyle
            putEditorDyn ws

            withCurrentBuffer $ do
                delOverlaysOfOwnerB "make"
                V.mapM_
                    (addOverlayB <=< messageToOverlayB style)
                    (fold warningsByBuffer)

            printMsg (case code of
                ExitSuccess -> "pyflakes finished successfully."
                ExitFailure f -> "pyflakes failed with code " <> T.pack (show f))