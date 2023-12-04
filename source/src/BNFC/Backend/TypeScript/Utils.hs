module BNFC.Backend.TypeScript.Utils where

import BNFC.CF (Cat (TokenCat, ListCat), catToStr, Data, isTokenCat)
import BNFC.Utils (mkName, NameStyle (MixedCase))
import Data.List (nubBy)

-- wrap string into single quotes
wrapSQ :: String -> String
wrapSQ str = "'" ++ str ++ "'"

-- convert category name to TS type name
catToTypeName :: Cat -> String
catToTypeName cat = case cat of 
  TokenCat c -> c ++ "Token"
  ListCat c  -> catToTypeName c ++ "List"
  c          -> show c

indentStr :: Int -> String -> String
indentStr size = (replicate size ' ' ++)

withOccurences :: Eq a => [a] -> [(a, Int, Bool)]
withOccurences list = reverse $ map (\(a, occ) -> (a, occ - 1, totalOccurences a == 1)) indexedArr
  where
    totalOccurences = flip getOccurences list

    indexedArr = foldl foldFn [] list
    foldFn list item = (item, newCount) : list
      where
        newCount = getOccurences item (map fst list) + 1

getOccurences :: Eq a => a -> [a] -> Int
getOccurences item list = length $ filter (==item) list

catToTsType :: Cat -> String
catToTsType (ListCat c) = "Array<" ++ listItem ++ ">"
  where
    listItem = toMixedCase (catToTsType c)

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

