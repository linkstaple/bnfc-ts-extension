module BNFC.Backend.TypeScript.Utils where

import Text.PrettyPrint (Doc, text)

import Data.Char (toLower)

import BNFC.CF (Cat (TokenCat, ListCat), catToStr, normCat, Data, CF, isList, getAbstractSyntax, literals)
import BNFC.Utils (mkName, NameStyle (OrigCase, MixedCase), mkNames)
import BNFC.Backend.Common.NamedVariables (getVars, firstLowerCase)

-- | wrap string into single quotes.
wrapSQ :: String -> String
wrapSQ str = "'" ++ str ++ "'"

-- | indent string with N spaces.
indentStr :: Int -> String -> String
indentStr size = (replicate size ' ' ++)

-- | indent string with N spaces and transform to Doc.
indent :: Int -> String -> Doc
indent size str = text (indentStr size str)

-- | derive name for TS type from category.
catToTsType :: Cat -> String
catToTsType (ListCat c) = "Array<" ++ catToTsType c ++ ">"
catToTsType (TokenCat c) = toMixedCase (c ++ "Token")
catToTsType cat = toMixedCase (catToStr cat)

-- | reserved TS keywords.
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
  , "switch"
  , "case"
  , "break"
  ]

toMixedCase :: String -> String
toMixedCase = firstLowerCase . mkName reservedKeywords MixedCase

-- | produces a type name for rule label
mkTypeName :: String -> String
mkTypeName = mkName reservedKeywords OrigCase

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

-- | get variable names which will be used in node structure
-- for categories used in production rule.
getVarsFromCats :: [Cat] -> [String]
getVarsFromCats cats = mkNames ["type"] OrigCase normalizedVars
  where
    normalizedCats = map normCat cats
    indexedVars = getVars normalizedCats

    normalizeVar :: (String, Int) -> String
    normalizeVar (varName, idx) = map toLower varName ++ varNameSuffix
      where
        varNameSuffix = if idx == 0 then "" else "_" ++ show idx
    
    normalizedVars = map normalizeVar indexedVars

-- | we don't need to declare nodes, which will represent list
-- because they will be referenced directly with TS type Array<SomeType>.
getAbsynWithoutLists :: CF -> [Data]
getAbsynWithoutLists = filter (not . isList . fst) . getAbstractSyntax

-- | get used tokens represented as cats
getAllTokenCats :: CF -> [Cat]
getAllTokenCats cf = map TokenCat (literals cf)

-- | get TS type names for all tokens
getAllTokenTypenames :: CF -> [String]
getAllTokenTypenames cf = map catToTsType (getAllTokenCats cf)

mkTokenNodeName :: String -> String
mkTokenNodeName tokenName = wrapSQ (tokenName ++ "Token")
