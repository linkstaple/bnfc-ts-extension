module BNFC.Backend.TypeScript.Utils where

import BNFC.CF (Cat (TokenCat, ListCat), catToStr, normCat, Data, CF, isList, getAbstractSyntax)
import BNFC.Utils (mkName, NameStyle (MixedCase, LowerCase), mkNames)
import Text.PrettyPrint (Doc, text)
import BNFC.Backend.Common.NamedVariables (getVars)
import Data.Char (toLower, isDigit)

-- wrap string into single quotes
wrapSQ :: String -> String
wrapSQ str = "'" ++ str ++ "'"

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

getVarsFromCats :: [Cat] -> [String]
-- "type" is reserved for identification of AST node
getVarsFromCats cats = map normalizeSuffix varNames
  where
    normalizedCats = map normCat cats
    indexedVars = getVars normalizedCats

    normalizeVar :: (String, Int) -> String
    normalizeVar (varName, idx) = map toLower varName ++ varNameSuffix
      where
        varNameSuffix = if idx == 0 then "" else "_" ++ show idx
    
    normalizedVars = map normalizeVar indexedVars
    varNames = mkNames ["type"] LowerCase normalizedVars

    normalizeSuffix :: String -> String
    normalizeSuffix varName = if isDigit lastChar then init varName ++ "_" ++ [lastChar] else varName
      where
        lastChar = last varName

getAbsynWithoutLists :: CF -> [Data]
getAbsynWithoutLists = filter (not . isList . fst) . getAbstractSyntax

mkTokenNodeName :: String -> String
mkTokenNodeName tokenName = wrapSQ (tokenName ++ "Token")
