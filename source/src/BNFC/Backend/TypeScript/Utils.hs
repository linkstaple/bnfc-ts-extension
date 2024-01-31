module BNFC.Backend.TypeScript.Utils where

import BNFC.CF (Cat (TokenCat, ListCat), catToStr, Data, isTokenCat)
import BNFC.Utils (mkName, NameStyle (MixedCase))
import Data.List (nubBy)
import Text.PrettyPrint (Doc, text)

-- wrap string into single quotes
wrapSQ :: String -> String
wrapSQ str = "'" ++ str ++ "'"

-- convert category name to TS type name
catToTypeName :: Cat -> String
catToTypeName cat = case cat of 
  TokenCat c -> c ++ "Token"
  ListCat c  -> catToTypeName c ++ "List"
  c          -> show c

-- indent string with N spaces
indentStr :: Int -> String -> String
indentStr size = (replicate size ' ' ++)

indent :: Int -> String -> Doc
indent size str = text (indentStr size str)

catToTsType :: Cat -> String
catToTsType (ListCat c) = "Array<" ++ catToTsType c ++ ">"
catToTsType (TokenCat c) = toMixedCase (c ++ "Token")
catToTsType cat = toMixedCase (catToStr cat)

reservedKeywords :: [String]
reservedKeywords =
  [ "type"
  , "class"
  , "const"
  , "var"
  , "export"
  , "default"
  , "import"
  , "function"
  , "number"
  , "string"
  , "undefined"
  , "null"
  , "if"
  , "else"
  , "try"
  , "catch"
  , "throw"
  , "let"
  , "yield"
  , "for"
  , "do"
  , "while"
  , "of"
  , "in"
  , "delete"
  , "return"
  , "typeof"
  ]

toMixedCase :: String -> String
toMixedCase = mkName reservedKeywords MixedCase

reservedTokenCats :: [Cat]
reservedTokenCats =
  [ TokenCat "String"
  , TokenCat "Ident"
  , TokenCat "Char"
  , TokenCat "Integer"
  , TokenCat "Double"
  ]

reservedTokenNames :: [String]
reservedTokenNames = map catToStr reservedTokenCats

getTokenCats :: [Data] -> [Cat]
getTokenCats datas = nubBy tokenCatComparator allTokenCats
  where
    allTokenCats = reservedTokenCats ++ allUserTokenCats
    allUserTokenCats = filter isTokenCat allCats
    allCats = concatMap (concatMap snd . snd) datas

    tokenCatComparator :: Cat -> Cat -> Bool
    tokenCatComparator (TokenCat c1) (TokenCat c2) = c1 == c2
    tokenCatComparator _ _ = False

