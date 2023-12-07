{-# LANGUAGE TupleSections #-}

module BNFC.Backend.TypeScript.CFtoBuilder where

import Text.PrettyPrint.HughesPJClass (Doc, text, vcat)
import Data.Bifunctor (Bifunctor(second, first))
import Data.List (intercalate, nubBy, nub)

import BNFC.Utils ((+++), camelCase_, mixedCase_)
import BNFC.CF (CF, Cat (ListCat, TokenCat, Cat), identCat, isList, IsFun (isNilFun, isOneFun, isConsFun), getAbstractSyntax, Data, catOfList, isTokenCat, catToStr, ruleGroups, Rule, Rul (rhsRule, funRule), SentForm, RString, WithPosition (wpThing))
import BNFC.Backend.TypeScript.Utils (indentStr, wrapSQ, withOccurences, getTokenCats, catToTsType, toMixedCase, reservedTokenCats)
import BNFC.Options (SharedOptions (lang))
import BNFC.Backend.Antlr.CFtoAntlr4Parser (catToNT, antlrRuleLabel)
import Data.Either (lefts)

type DataGroup = (Cat, [(RString, SentForm)])

cfToBuilder :: CF -> SharedOptions -> Doc
cfToBuilder cf opts = vcat $ concat
  [ ["import {TerminalNode} from 'antlr4'"]
  , importDecls
  , [text ""]
  , tokenDecls
  , [text ""]
  , buildFnDecls
  ]
    where
      datas = getAbstractSyntax cf
      importDecls = map text $ mkImportDecls fullData (lang opts)

      tokenDecls = map text $ intercalate [""] $ map mkBuildTokenFunction allTokenCats
      buildFnDecls = map text $ intercalate [""] (map mkBuildFunction datas)
      allTokenCats = getTokenCats datas

      groups = ruleGroups cf
      cats = map fst groups
      rhsRules = map (map rhsRule . snd) groups
      ruleLabels = map (map funRule . snd) groups
      labelsWithRhsRules = zipWith zip ruleLabels rhsRules
      fullData = zip cats labelsWithRhsRules

mkThrowErrorStmt :: String -> Cat -> String
mkThrowErrorStmt fnName cat = "throw '[" ++ fnName ++ "]" +++ "Error: arg should be an instance of" +++ identCat cat ++ "Context" ++ "'"

mkBuildFunction :: Data -> [String]
mkBuildFunction (cat, ruleDescription) = concat
  [ ["export function" +++ fnName ++ "(arg: " ++ argCtxType ++ "): " ++ returnType +++ "{"]
  , map (indentStr 2) fnBody
  , [indentStr 2 $ mkThrowErrorStmt fnName cat]
  , ["}"]
  ]
  where
    fnName = mkBuildFnName cat
    argCtxType = camelCase_ $ identCat cat ++ "_context"
    returnType = catToTsType cat
    labels = map fst ruleDescription
    catid = identCat cat

    fnBody :: [String] = if isList cat
      then concat listBody
      else concat body

    listItemCat = catOfList cat
    listBody = map (\rule -> if isNilFun rule
      then mkEmptyListCheck
      else if isConsFun rule
        then mkConsListCheck listItemCat
        else if isOneFun rule
          then mkOneListCheck listItemCat
          else []) labels

    mkConsListCheck itemCat =
        [ "if (arg instanceof" +++ catid ++ "_PrependFirstContext) {"
        , indentStr 2 $ "const item = arg." ++ catToNT itemCat ++ "()"
        , indentStr 2 $ "const list = arg." ++ catToNT cat ++ "()"
        , indentStr 2 $ "const data =" +++ buildItemFn ++ "(item)"
        , indentStr 2 $ "return [data].concat(" ++ fnName ++ "(" ++ "list" ++ ")"++ ")"
        , "}"
        ]
      where
        buildItemFn = mkBuildFnName itemCat

    mkOneListCheck itemCat =
        [ "if (arg instanceof" +++ catid ++ "_AppendLastContext) {"
        , indentStr 2 $ "const item = arg." ++ catToNT itemCat ++ "()"
        , indentStr 2 $ "const data =" +++ buildItemFn ++ "(item)"
        , indentStr 2 "return [data]"
        , "}"
        ]
      where
        buildItemFn = mkBuildFnName itemCat

    mkEmptyListCheck =
      [ "if (arg instanceof" +++ catid ++ "_EmptyContext) {"
      , indentStr 2 "return []"
      , "}"
      ]

    ctxList = getCtxList (excludeValues (cat, ruleDescription))
    ruleValues = map snd ruleDescription
    ruleLabels = map fst ruleDescription
    bodyContentData = zip3 ruleLabels ctxList ruleValues

    buildConditionBody :: (String, String, [Cat]) -> [String]
    buildConditionBody (ruleName, ctxName, cats) =
        [ "if (arg instanceof" +++ ctxName ++ ") {" ]
        ++ map (indentStr 2 . generateAssignment) indexedCatValues
        ++
        [ indentStr 2 "return {"
        , indentStr 4 $ "type:" +++ wrapSQ ruleName ++ ","
        ]
        ++ map ((++ ",") . indentStr 4 . ("value_"++) . show) (take (length cats) [1..]) -- value_n,
        ++
        [ indentStr 2 "}"
        , "}"
        ]
      where
        indexedCatValues = zip [1..] (withOccurences cats)
        generateAssignment (idx, (cat, occ, isSingle)) =
            "const value_" ++ show idx ++ " = " ++ mkBuildFnName cat ++ "(arg." ++ catToNT cat ++ "(" ++ argValue ++ "))"
          where
            argValue = if isSingle then "" else show occ

    body = map buildConditionBody bodyContentData

mkBuildTokenFunction :: Cat -> [String]
mkBuildTokenFunction tokenCat =
  [ "export function" +++ fnName ++ "(arg: TerminalNode):" +++ returnType +++ "{"
  , indentStr 2 "return {"
  , indentStr 4 $ "type:" +++ wrapSQ (tokenName ++ "Token") ++ ","
  , indentStr 4 $ "value:" +++ value
  , indentStr 2 "}"
  , "}"
  ]
  where
    tokenName = catToStr tokenCat
    fnName = mkBuildFnName tokenCat
    returnType = catToTsType tokenCat
    value = case tokenName of
      "Integer" -> "parseInt(arg.getText())"
      "Double"  -> "parseFloat(arg.getText())"
      _         -> "arg.getText()"

-- maybe change somehow
-- works only for cat ctx, not rule
mkCtxType :: (Cat, [String]) -> String
mkCtxType (cat, ruleLabels) = case cat of
    (ListCat _) -> intercalate ", " $ mkCtxName (Left cat) : map (listLabelMapper cat) ruleLabels
    c          -> intercalate ", " $ mkCtxName (Left c) : map (mkCtxName . Right) ruleLabels
  where
    listLabelMapper :: Cat -> String -> String
    listLabelMapper cat label = camelCase_ (identCat cat) ++ result label
      where result label
              | isNilFun label  = "_EmptyContext"
              | isConsFun label = "_PrependFirstContext"
              | isOneFun label  = "_AppendLastContext"
              | otherwise       = "_UnknownListContext"

    mkCtxName :: Either Cat String -> String
    mkCtxName (Left cat) = camelCase_ $ identCat cat ++ "_context"
    mkCtxName (Right label) = camelCase_ $ label ++ "_context"

excludeValues :: Data -> (Cat, [String])
excludeValues = second (map fst)

getCtxList :: (Cat, [String]) -> [String]
getCtxList (cat, rules) = case cat of
    (ListCat c) -> map (mapListType c) rules
    _           -> map (camelCase_ . (++"_Context")) rules
  where
    mapListType cat rule
      | isNilFun rule  = camelCase_ (identCat cat) ++ "_EmptyContext"
      | isConsFun rule = camelCase_ (identCat cat) ++ "_PrependFirstContext"
      | isOneFun rule  = camelCase_ (identCat cat) ++ "_AppendLastContext"
      | otherwise      = ""

mkBuildFnName :: Cat -> String
mkBuildFnName cat = mixedCase_ ("build_" ++ restName)
  where
    restName = case cat of
      ListCat cat  -> catToStr cat ++ "List"
      TokenCat cat -> cat ++ "Token"
      otherCat     -> catToStr otherCat

mkImportDecls :: [DataGroup] -> String -> [String]
mkImportDecls groups lang = [ctxImportStmt, astImportStmt]
  where
    groupsWithoutPos = map (second (map (first wpThing))) groups
    ctxNames = concatMap (\(cat, rules) -> identCat cat : map (antlrRuleLabel cat . fst) rules) groupsWithoutPos
    ctxImports = intercalate ", " $ nub $ map (++ "Context") ctxNames

    tokenImports = map catToTsType (getTokenCatsFromGroup groups)
    astNames = nub $ tokenImports ++ (concatMap (\(cat, rules) -> catToTsType cat : map (toMixedCase . fst) rules) $ filter (isUsualCat . fst) groupsWithoutPos)
    astImports = intercalate ", " (filter (not . null) astNames)

    ctxImportStmt = "import {" ++ ctxImports ++ "} from './" ++ parserFile ++ "'"
    astImportStmt = "import {" ++ astImports ++ "} from './abstract'"

    parserFile = camelCase_ $ lang ++ "Parser"

    isUsualCat (Cat _) = True
    isUsualCat _       = False

getTokenCatsFromGroup :: [DataGroup] -> [Cat]
getTokenCatsFromGroup groups = nubBy tokenCatComparator allTokenCats
  where
    allTokenCats = reservedTokenCats ++ allUserTokenCats
    allUserTokenCats = filter isTokenCat allCats
    allCats = concatMap (concatMap (lefts . snd) . snd) groups

    tokenCatComparator :: Cat -> Cat -> Bool
    tokenCatComparator (TokenCat c1) (TokenCat c2) = c1 == c2
    tokenCatComparator _ _ = False
