
module IdrisIDESexpCopyPasteFromIdris
    ( receiveString
    ) where

import Control.Applicative
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.UTF8 as UTF8
import Idris.IdeMode
import Numeric
import Text.Trifecta hiding (Err)
import Text.Trifecta.Delta

receiveString :: String -> Either String SExp
receiveString x =
  case parseSExp x of
    Failure _ -> Left "parse failure"
    Success r -> Right r

parseSExp :: String -> Result SExp
parseSExp = parseString pSExp (Directed (UTF8.fromString "(unknown)") 0 0 0 0)

pSExp = do xs <- between (char '(') (char ')') (pSExp `sepBy` (char ' '))
           return (SexpList xs)
    <|> atom

atom = do string "nil"; return (SexpList [])
   <|> do char ':'; x <- atomC; return x
   <|> do char '"'; xs <- many quotedChar; char '"'; return (StringAtom xs)
   <|> do ints <- some digit
          case readDec ints of
            ((num, ""):_) -> return (IntegerAtom (toInteger num))
            _ -> return (StringAtom ints)

atomC = do string "True"; return (BoolAtom True)
    <|> do string "False"; return (BoolAtom False)
    <|> do xs <- many (noneOf " \n\t\r\"()"); return (SymbolAtom xs)

quotedChar = try (string "\\\\" >> return '\\')
         <|> try (string "\\\"" >> return '"')
         <|> noneOf "\""