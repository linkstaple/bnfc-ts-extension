module BNFC.Backend.TypeScript.CFtoAbstract (cfToAbstract) where

import BNFC.CF (CF, Data, Cat (TokenCat), catInteger, catDouble, literals)
import BNFC.Utils ( (+++) )
import BNFC.Backend.TypeScript.Utils (wrapSQ, indentStr, toMixedCase, catToTsType, indent, getVarsFromCats, mkTokenNodeName, getAbsynWithoutLists)

import Text.PrettyPrint.HughesPJClass (Doc, text, vcat)
import Data.List (intercalate, intersperse)

type TypeName = String

cfToAbstract :: CF -> Doc
cfToAbstract cf = vcat $ concat
    [ tokensDecl
    , [text ""]
    , intersperse (text "") abstractNodes
    ]
  where
    -- we don't need to declare list types, because they will be
    -- referenced directly with Array<SomeType>
    absynData = getAbsynWithoutLists cf
    abstractNodes = map dataToAbstract absynData

    allTokenNames = literals cf
    tokensDecl = concatMap (map text . mkTokenDecl) allTokenNames

dataToAbstract :: Data -> Doc
dataToAbstract (cat, rules) = vcat
    [ mkUnion typeName ruleNames
    , ""
    , rulesDecl
    ]
  where
    typeName = catToTsType cat
    ruleNames = map (toMixedCase . fst) rules
    rulesDecl = vcat $ intersperse (text "") $ map mkRuleDecl rules

mkRuleDecl :: (String, [Cat]) -> Doc
mkRuleDecl (ruleName, cats) = vcat $ concat
    [ [ text $ "export type" +++ typeName ++ " = {"
      , indent 2 $ "type:" +++ wrapSQ ruleName
      ]
    , valuesList
    , ["}"]
    ]
  where
    valuesList = map (indent 2) $ zipWith (\varName varType -> concat [varName, ": ", varType]) varNames varTypes
    typeName = toMixedCase ruleName
    varNames = getVarsFromCats cats
    varTypes = map catToTsType cats

-- makes TypeScript union
-- mkUnion "Either" ["Left", "Right"] --> type Either = Left | Right
mkUnion :: TypeName -> [TypeName] -> Doc
mkUnion typeName typeNames = text typeDecl
  where
    typeDecl = "export type" +++ typeName ++ " = " ++ intercalate " | " typeNames

-- valueType is a string which represents TS basic type
mkTokenDecl :: String -> [String]
mkTokenDecl tokenName =
    [ "export type" +++ catToTsType (TokenCat tokenName) ++ " = {"
    , indentStr 2 $ "type: " ++ mkTokenNodeName tokenName
    , indentStr 2 $ "value: " ++ value
    , "}"
    ]

  where
    value = if tokenName `elem` [catInteger, catDouble] then "number" else "string"
