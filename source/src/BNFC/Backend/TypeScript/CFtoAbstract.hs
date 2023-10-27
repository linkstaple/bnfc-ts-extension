module BNFC.Backend.TypeScript.CFtoAbstract where

import Text.PrettyPrint.HughesPJClass (Doc, text, vcat)
import Data.List (intercalate, intersperse)

import BNFC.CF (CF, Cat (TokenCat, ListCat), Data, getAbstractSyntax, IsFun (isNilFun, isConsFun, isOneFun), isTokenCat, catToStr)
import BNFC.Utils

type TypeName = String

cfToAbstract :: CF -> Doc
cfToAbstract cf = vcat $ concat
    [ predefinedTokens
    , userDefinedTokens
    , [text ""]
    , intersperse (text "") abstractNodes
    ]
  where
    datas = getAbstractSyntax cf
    abstractNodes = map dataToAbstract datas

    -- defining tokens denoted with 'token' LBNF keyword
    shouldCreateToken :: Cat -> Bool
    shouldCreateToken cat = isTokenCat cat && notElem catStr reservedTokenNames
      where
        catStr = catToStr cat

    tokenCats = filter shouldCreateToken $ concat $ concatMap (map snd . snd) datas
    userDefinedTokens = map (\cat -> mkToken (catToType cat, "string")) tokenCats

dataToAbstract :: Data -> Doc
dataToAbstract (cat, rules) = vcat
    [ mkUnion typeName ruleNames
    , ""
    , rulesDecl
    ]
  where
    typeName = catToType cat
    ruleNames = map (ruleNameToType cat . fst) rules
    rulesDecl = vcat $ intersperse (text "") $ map (ruleToType cat) rules

ruleToType :: Cat -> (String, [Cat]) -> Doc
ruleToType cat (ruleName, cats) = vcat $ concat
    [ [ text $ "type" +++ mkTypeName typeName ++ " = {"
      , indent 2 $ "type: " ++ "'" ++ typeName ++ "'"
      ]
    , valuesList
    , ["}"]
    ]
  where
    valuesList = map (indent 2) $ zipWith (\num cat -> "value_" ++ show num ++ ": " ++ mkTypeName (catToType cat)) [1..] cats
    typeName = ruleNameToType cat ruleName

-- makes TypeScript union
-- mkUnion "Either" ["Left", "Right"] --> type Either = Left | Right
mkUnion :: TypeName -> [TypeName] -> Doc
mkUnion typeName types = text typeDecl
  where
    typeDecl = "type" +++ mkTypeName typeName ++ " = " ++ intercalate " | " typeNames
    typeNames = map mkTypeName types

-- indent string with N spaces
indent :: Int -> String -> Doc
indent size = text . (replicate size ' ' ++)

-- valueType is a string which represents TS basic type
mkToken :: (String, String) -> Doc
mkToken (tokenName, valueType) = vcat
  [ text $ "type" +++ mkTypeName tokenName ++ " = {"
  , indent 2 $ "type: " ++ wrapSQ tokenName
  , indent 2 $ "value: " ++ valueType
  , "}"
  ]

reservedTokenNames :: [String]
reservedTokenNames =
  [ "String"
  , "Ident"
  , "Char"
  , "Integer"
  , "Double"
  ]

predefinedTokens :: [Doc]
predefinedTokens = tokens
  where
    tokens = zipWith
            (curry mkToken) (map (++ "Token") reservedTokenNames)
            ["string", "string", "string", "number", "number"]

-- wrap string into single quotes
wrapSQ :: String -> String
wrapSQ str = "'" ++ str ++ "'"

-- convert category name to TS type name
catToType :: Cat -> String
catToType (TokenCat c) = c ++ "Token"
catToType (ListCat c) = catToType c ++ "List"
catToType c = show c

-- convert rule label to TS type name
-- ruleNameToType (ListCat (Cat "Command")) "([])" --> "CommandListEmpty"
ruleNameToType :: Cat -> String -> String
ruleNameToType cat rule 
    | isNilFun  rule = catName ++ "Empty"
    | isConsFun rule = catName ++ "Cons"
    | isOneFun  rule = catName ++ "Single"
    | otherwise      = rule
  where
    catName = catToType cat

mkTypeName :: String -> String
mkTypeName = mkName reservedKeywords MixedCase

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
  ]
  