{-# LANGUAGE TupleSections #-}

module BNFC.Backend.TypeScript.CFtoBuilder where

import Text.PrettyPrint.HughesPJClass (Doc, text, vcat)
import Data.Bifunctor (Bifunctor(second, first))
import Data.List (intercalate, nubBy, nub)

import BNFC.Utils ((+++), camelCase_)
import BNFC.CF (CF, Cat (ListCat, TokenCat, Cat), identCat, isList, IsFun (isNilFun, isOneFun, isConsFun, isCoercion), getAbstractSyntax, isTokenCat, catToStr, ruleGroups, Rul (rhsRule, funRule), SentForm, RString, WithPosition (wpThing))
import BNFC.Backend.TypeScript.Utils (indentStr, wrapSQ, getTokenCats, catToTsType, reservedTokenCats)
import BNFC.Options (SharedOptions (lang))
import BNFC.Backend.Antlr.CFtoAntlr4Parser (antlrRuleLabel, makeLeftRecRule)
import Data.Either (lefts)
import Data.Maybe (mapMaybe)
import Data.Char (toUpper)

type DataGroup = (Cat, [(RString, SentForm)])

cfToBuilder :: CF -> SharedOptions -> Doc
cfToBuilder cf opts = vcat $ concat
  [ ["import {Token} from 'antlr4'"]
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
      buildFnDecls = map text $ intercalate [""] (map mkBuildFunction fullData)
      allTokenCats = getTokenCats datas

      groups = map (second (map (makeLeftRecRule cf))) $ ruleGroups cf
      cats = map fst groups
      rhsRules = map (map rhsRule . snd) groups
      ruleLabels = map (map funRule . snd) groups
      labelsWithRhsRules = zipWith zip ruleLabels rhsRules
      fullData = zip cats labelsWithRhsRules

mkThrowErrorStmt :: Cat -> String
mkThrowErrorStmt cat = "throw new Error('[" ++ mkBuildFnName cat ++ "]" +++ "Error: arg should be an instance of" +++ camelCase_ (identCat cat) ++ "Context" ++ "')"

mkBuildTokenFunction :: Cat -> [String]
mkBuildTokenFunction tokenCat =
  [ "export function" +++ fnName ++ "(arg: Token):" +++ returnType +++ "{"
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
      "Integer" -> "parseInt(arg.text)"
      "Double"  -> "parseFloat(arg.text)"
      _         -> "arg.text"

mkBuildFnName :: Cat -> String
mkBuildFnName cat = "build" ++ firstUpperCase restName
  where
    restName = case cat of
      ListCat cat  -> catToStr cat ++ "List"
      TokenCat cat -> cat ++ "Token"
      otherCat     -> catToStr otherCat

mkImportDecls :: [DataGroup] -> String -> [String]
mkImportDecls groups lang = [ctxImportStmt, astImportStmt]
  where
    groupsWithoutPos = map (second (map (first wpThing))) groups
    ctxNames = concatMap (\(cat, rules) -> identCat cat : zipWith (\(fun, _) idx -> antlrRuleLabel cat fun (Just idx)) rules [1..]) groupsWithoutPos
    ctxImports = intercalate ", " $ nub $ map (++ "Context") ctxNames

    tokenImports = map catToTsType (getTokenCatsFromGroup groups)
    astNames = nub $ tokenImports ++ (map (\(cat, _) -> catToTsType cat) $ filter (isUsualCat . fst) groupsWithoutPos)
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

mkBuildFunction :: DataGroup -> [String]
mkBuildFunction (cat, rhsRules) =
  [ "export function" +++ mkBuildFnName cat ++ "(arg: " ++ identCat cat ++ "Context):" +++ catToTsType cat +++ "{"]
  ++
  concatMap (uncurry mkIfStmt) datas
  ++
  [ indentStr 2 $ mkThrowErrorStmt cat
  , "}"
  ]
  where
    datas = zipWith (\rhsRule index -> (first wpThing rhsRule, index)) rhsRules [1..]

    mkIfStmt :: (String, SentForm) -> Integer -> [String]
    mkIfStmt (ruleLabel, rhsRule) ifIdx =
        [indentStr 2 $ "if (arg instanceof" +++ antlrRuleLabel cat ruleLabel antlrRuleLabelIdx ++ "Context) {"]
        ++
        mkIfBody ruleLabel
        ++
        [indentStr 2 "}"]

      where
        antlrRuleLabelIdx = if isCoercion ruleLabel then Just ifIdx else Nothing
        rhsRuleWithIdx = mapMaybe (\(rule, idx) -> either (\cat -> Just (cat, idx)) (\_ -> Nothing) rule) $ zip rhsRule [1..]
        mkPattern idx = "_p_" ++ show ifIdx ++ "_" ++ show idx

        mkIfBody ruleLabel
          | isCoercion ruleLabel = map (\(cat, idx) -> indentStr 4 $ "return" +++ mkBuildFnName cat ++ "(arg." ++ mkPattern idx ++ ")") rhsRuleWithIdx
          | isNilFun ruleLabel   = emptyListBody
          | isOneFun ruleLabel   = oneListBody
          | isConsFun ruleLabel  = consListBody
          | otherwise            =
            concat
              [ zipWith
             (\ (cat, idx) valueIndex
                -> indentStr 4
                     $ "const value_" ++ show valueIndex
                         +++ "=" +++ mkBuildFnName cat ++ "(arg." ++ mkPattern idx ++ ")")
                          rhsRuleWithIdx [1 .. ]
              , [ indentStr 4 "return {"]
              , [ indentStr 6 $ "type: " ++ wrapSQ ruleLabel ++ ","]
              , map
                  (\ valueIndex -> indentStr 6 ("value_" ++ show valueIndex ++ ","))
                  [1 .. length rhsRuleWithIdx]
              , [ indentStr 4 "}" ]
              ]

        emptyListBody = [indentStr 4 "return []"]
        oneListBody = map (\(cat, idx) -> indentStr 4 $ "const data =" +++ mkBuildFnName cat ++ "(arg." ++ mkPattern idx ++ ")") rhsRuleWithIdx ++ [ indentStr 4 "return [data]"]
        consListBody =
            [ indentStr 4 $ "const value1 =" +++  mkBuildFnName firstCat ++ "(arg." ++ mkPattern firstIdx ++ ")"
            , indentStr 4 $ "const value2 =" +++  mkBuildFnName secondCat ++ "(arg." ++ mkPattern secondIdx ++ ")"
            , indentStr 4 $ "return" +++ resultList
            ]
          where
            (firstCat, firstIdx) = head rhsRuleWithIdx
            (secondCat, secondIdx) = rhsRuleWithIdx !! 1
            (itemVar, listVar) = if isList firstCat then ("value2", "value1") else ("value1", "value2")
            resultList = if isList firstCat
              then
                "[..." ++ listVar ++ ", " ++ itemVar ++ "]"
              else
                "[" ++ itemVar ++ ", ..." ++ listVar ++ "]"

firstUpperCase :: String -> String
firstUpperCase "" = ""
firstUpperCase (a:b) = toUpper a : b
