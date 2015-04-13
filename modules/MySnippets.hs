{-# LANGUAGE OverloadedStrings #-}

module MySnippets
    ( mySnippets
    ) where

import Control.Applicative

import Yi
import qualified Yi.Rope as R

import Snippet

mySnippets :: YiM [Snippet]
mySnippets = do
    basename <- return "MyFile"
    return
        [ Snippet "haskell module" $ do
            lit "module " >> place basename >> nl
            lit "    (" >> finish >> nl
            lit "    ) where"
        , Snippet "haskell LANGUAGE pragma" $ do
            lit "{-# LANGUAGE "
            place "OverloadedStrings"
            line " #-}"
        , Snippet "haskell import qualified" $ do
            lit "import qualified "
            moduleName <- place "Data.Map.Strict"
            lit " as "
            abbrev <- R.filter (`elem` ['A'..'Z']) <$> refer moduleName
            line abbrev
        , Snippet "python main" $ do
            line "def main():\n    "
            finish
            nl
            nl
            line "if __name__ == '__main__':"
            line "    main()"
        ]

objcClass :: Snippet
objcClass = Snippet "objc class" $ do
    className <-
        lit "@interface " *> place "ShinyClass" <* lit ": NSObject" <* nl
    line "@end"
    nl
    lit "@implementation " >> mirror className >> nl
    line "@end"
