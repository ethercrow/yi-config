
--
--  This file aims to provide (the essential subset of) the same functionality
--  that vim plugins ctrlp and command-t provide.
--
--  Setup:
--
--    Add something like this to your config:
--
--      (ctrlCh 'p' ?>>! fuzzyFile)
--

module FuzzyFile
    ( fuzzyFile
    ) where

import Control.Monad
import Control.Monad.Base
import Control.Monad.State
import Data.List (isInfixOf, isSuffixOf)
import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

import Yi
import Yi.Types

import Fuzzy

fuzzyFile :: YiM ()
fuzzyFile = genericFuzzy =<< getItems

fileItem :: FilePath -> FuzzyItem
fileItem filename = FuzzyItem
    { fiAction = void (editFile filename)
    , fiToText = T.pack ("File " <> filename)
    }

bufferItem :: BufferId -> FuzzyItem
bufferItem bufId = FuzzyItem
    { fiAction = withEditor $ do
        bufs <- gets (M.assocs . buffers)
        case filter ((== bufId) . ident . attributes . snd) bufs of
            [] -> error ("Couldn't find buffer" <> show bufId)
            (bufRef, _) : _ -> switchToBufferE bufRef
    , fiToText = T.pack "Buffer " <> case bufId of
        MemBuffer x -> x
        FileBuffer x -> T.pack x
    }

getItems :: YiM [FuzzyItem]
getItems = do
    fileList <-
        fmap
            (fmap fileItem)
            (liftBase (getRecursiveContents "."))
    bufList <-
        fmap
            (fmap (bufferItem . ident . attributes))
            (withEditor (gets (M.elems . buffers)))
    return (fileList <> bufList)

-- shamelessly stolen from Chapter 9 of Real World Haskell
-- takes about 3 seconds to traverse linux kernel, which is not too outrageous
-- TODO: check if it works at all with cyclic links
-- TODO: perform in background, limit file count or directory depth
-- TODO: ignore what is in .gitignore
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter predicate names
        predicate fileName = and
            [ fileName `notElem`
                [ "."
                , ".."
                , ".git"
                , ".svn"
                , "node_modules"
                , "_build"
                , ".build"
                , "Packages"
                , ".stack-work"
                , ".tox"
                , ".stack-work"
                , ".build"
                , ".compiled"
                , "dist-newstyle"
                ]
            , not (".hi" `isSuffixOf` fileName)
            , not ("-boot" `isSuffixOf` fileName)
            , not (".dyn_hi" `isSuffixOf` fileName)
            , not (".dyn_o" `isSuffixOf` fileName)
            , not (".p_hi" `isSuffixOf` fileName)
            , not (".p_o" `isSuffixOf` fileName)
            , not (".o" `isSuffixOf` fileName)
            , not (".swp" `isSuffixOf` fileName)
            , not (".beam" `isSuffixOf` fileName)
            , not (".pyc" `isSuffixOf` fileName)
            , not ("~" `isSuffixOf` fileName)
            , not ("dist/build/" `isInfixOf` fileName)
            , not (".eunit" `isInfixOf` fileName)
            ]
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return [path]
    return (concat paths)