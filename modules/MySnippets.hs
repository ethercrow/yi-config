{-# LANGUAGE OverloadedStrings #-}

module MySnippets
    ( mySnippets
    ) where

import Control.Applicative

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
    , Snippet "testsuite" $ do
        lit "Test-Suite " >> place "TestMain" >> nl
        line "  type: exitcode-stdio-1.0"
        lit "  main-is " >> place "TestMain.hs" >> nl
        line "  hs-source-dirs: src, test"
        line "  ghc-options: -Wall -ferror-spans"
        line "  default-language:    Haskell2010"
        line "  build-depends:"
        line "    base >= 4.6"
    , Snippet "cl" $ do
        className <-
            lit "@interface " *> place "ShinyClass" <* lit ": NSObject" <* nl
        line "@end"
        nl
        lit "@implementation " >> mirror className >> nl
        line "@end"
    ]