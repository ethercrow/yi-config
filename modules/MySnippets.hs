{-# LANGUAGE OverloadedStrings #-}

module MySnippets
    ( mySnippets
    ) where

import Control.Applicative

import Yi
import qualified Yi.Rope as R

import Snippet

mySnippets :: [Snippet]
mySnippets =
    [ Snippet "m" $ do
        lit "module " >> place "MyFile" >> nl
        lit "    (" >> finish >> nl
        lit "    ) where"
    , Snippet "lp" $ do
        lit "{-# LANGUAGE "
        _ <- place "OverloadedStrings"
        line " #-}"
    , Snippet "iq" $ do
        lit "import qualified "
        moduleName <- place "Data.Map.Strict"
        lit " as "
        abbrev <- R.filter (`elem` ['A'..'Z']) <$> refer moduleName
        line abbrev
    , Snippet "main" $ do
        line "def main():\n    "
        finish
        nl
        nl
        line "if __name__ == '__main__':"
        line "    main()"
    , Snippet "cl" $ do
        className <-
            lit "@interface " *> place "ShinyClass" <* lit ": NSObject" <* nl
        line "@end"
        nl
        lit "@implementation " >> mirror className >> nl
        line "@end"
    ]

