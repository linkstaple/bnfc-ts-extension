module BNFC.Backend.TypeScript.CFtoAbstract where

import Text.PrettyPrint.HughesPJClass (Doc, text, vcat)
import Data.List (intercalate, intersperse)

import BNFC.CF (CF, Data, getAbstractSyntax, catToStr, isList, Cat)
import BNFC.Utils
import BNFC.Backend.TypeScript.Utils (wrapSQ, indentStr, toMixedCase, getTokenCats, catToTsType)

type TypeName = String

cfToAbstract :: CF -> Doc
cfToAbstract cf = vcat $ concat
    [ tokensDecl
    , [text ""]
    , intersperse (text "") abstractNodes
    ]
  where
    datas = getAbstractSyntax cf
    -- we don't need to declare list types, because they will be
    -- referenced directly with Array<SomeType>
    datasWithoutLists = filter (not . isList . fst) datas
    abstractNodes = map dataToAbstract datasWithoutLists

    allTokenCats = getTokenCats datas
    tokensDecl = concatMap (map text . mkToken) allTokenCats

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
    valuesList = map (indent 2) $ zipWith (\num cat -> "value_" ++ show num ++ ": " ++ catToTsType cat) [1..] cats
    typeName = toMixedCase ruleName

-- makes TypeScript union
-- mkUnion "Either" ["Left", "Right"] --> type Either = Left | Right
mkUnion :: TypeName -> [TypeName] -> Doc
mkUnion typeName typeNames = text typeDecl
  where
    typeDecl = "export type" +++ typeName ++ " = " ++ intercalate " | " typeNames

-- indent string with N spaces
indent :: Int -> String -> Doc
indent size = text . (replicate size ' ' ++)

-- valueType is a string which represents TS basic type
mkToken :: Cat -> [String]
mkToken tokenCat =
    [ "export type" +++ catToTsType tokenCat ++ " = {"
    , indentStr 2 $ "type: " ++ wrapSQ (catToStr tokenCat ++ "Token")
    , indentStr 2 $ "value: " ++ value
    , "}"
    ]

  where
    value = if catToStr tokenCat `elem` ["Integer", "Double"] then "number" else "string"
