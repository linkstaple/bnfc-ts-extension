module BNFC.Backend.TypeScript.CFtoBuilder where

import Text.PrettyPrint.HughesPJClass (Doc, text, vcat)
import Data.Bifunctor (Bifunctor(second))
import Data.List (intercalate, nub)

import BNFC.Utils ((+++), camelCase_)
import BNFC.CF (CF, Cat (ListCat, TokenCat, Cat), identCat, isList, IsFun (isNilFun, isOneFun, isConsFun, isCoercion), catToStr, ruleGroups, Rul (rhsRule, funRule), SentForm, RString, WithPosition (wpThing), literals)
import BNFC.Backend.TypeScript.Utils (indentStr, wrapSQ, catToTsType, getVarsFromCats, mkTokenNodeName)
import BNFC.Options (SharedOptions (lang))
import BNFC.Backend.Antlr.CFtoAntlr4Parser (antlrRuleLabel, makeLeftRecRule)
import Data.Maybe (mapMaybe)
import BNFC.Backend.Common.NamedVariables (firstUpperCase)

type RuleData = (Cat, [(String, SentForm)])

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
      importDecls = map text $ mkImportDecls cf (lang opts)

      tokenDecls = map text $ intercalate [""] buildTokensFuns
      buildFnDecls = map text $ intercalate [""] buildFuns

      buildFuns =  map mkBuildFunction datas
      buildTokensFuns = map mkBuildTokenFunction allTokenCats

      allTokenCats = map TokenCat (literals cf)
      datas = cfToGroups cf

mkThrowErrorStmt :: Cat -> String
mkThrowErrorStmt cat = "throw new Error('[" ++ mkBuildFnName cat ++ "]" +++ "Error: arg should be an instance of" +++ camelCase_ (identCat cat) ++ "Context" ++ "')"

mkBuildTokenFunction :: Cat -> [String]
mkBuildTokenFunction tokenCat =
  [ "export function" +++ fnName ++ "(arg: Token):" +++ returnType +++ "{"
  , indentStr 2 "return {"
  , indentStr 4 $ "type:" +++ mkTokenNodeName tokenName ++ ","
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

mkImportDecls :: CF -> String -> [String]
mkImportDecls cf lang = [ctxImportStmt, astImportStmt]
  where
    groups = cfToGroups cf
    ctxNames = concatMap (\(cat, rules) -> identCat cat : zipWith (\(fun, _) idx -> antlrRuleLabel cat fun (Just idx)) rules [1..]) groups
    ctxImports = intercalate ", " $ nub $ map (++ "Context") ctxNames

    tokenImports = map (catToTsType . TokenCat) (literals cf)
    astNames = nub $ tokenImports ++ (map (\(cat, _) -> catToTsType cat) $ filter (isUsualCat . fst) groups)
    astImports = intercalate ", " (filter (not . null) astNames)

    ctxImportStmt = "import {" ++ ctxImports ++ "} from './" ++ parserFile ++ "'"
    astImportStmt = "import {" ++ astImports ++ "} from './abstract'"

    parserFile = camelCase_ $ lang ++ "Parser"

    isUsualCat (Cat _) = True
    isUsualCat _       = False

mkBuildFunction :: RuleData -> [String]
mkBuildFunction (cat, rulesWithLabels) =
  [ "export function" +++ mkBuildFnName cat ++ "(arg: " ++ identCat cat ++ "Context):" +++ catToTsType cat +++ "{"]
  ++
  concatMap (uncurry mkIfStmt) datas
  ++
  [ indentStr 2 $ mkThrowErrorStmt cat
  , "}"
  ]
  where
    datas = zip rulesWithLabels [1..]

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
              (\ (cat, idx) varName
                  -> indentStr 4
                      $ "const" +++ varName
                          +++ "=" +++ mkBuildFnName cat ++ "(arg." ++ mkPattern idx ++ ")")
                            rhsRuleWithIdx varNames
                , [ indentStr 4 "return {"]
                , [ indentStr 6 $ "type: " ++ wrapSQ ruleLabel ++ ","]
                , map
                    (\ varName -> indentStr 6 (varName ++ ","))
                    varNames
                , [ indentStr 4 "}" ]
                ]
            where
              varNames = getVarsFromCats rhsCats
              rhsCats = map fst rhsRuleWithIdx

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

cfToGroups :: CF -> [RuleData]
cfToGroups cf = map (second (map (ruleToData . makeLeftRecRule cf))) $ ruleGroups cf
  where
    ruleToData rule = ((wpThing . funRule) rule, rhsRule rule)
