module BNFC.Backend.TypeScript.CFtoBuilder where

import Data.Bifunctor (Bifunctor(second))
import Data.List (intercalate, nub, intersperse)
import Data.Maybe (mapMaybe)

import Text.PrettyPrint.HughesPJClass (Doc, text, vcat)

import BNFC.Utils ((+++), camelCase_)
import BNFC.CF (CF, Cat (ListCat, TokenCat, Cat), identCat, isList, IsFun (isNilFun, isOneFun, isConsFun, isCoercion), catToStr, ruleGroups, Rul (rhsRule, funRule), SentForm, WithPosition (wpThing))
import BNFC.Backend.TypeScript.Utils (indentStr, wrapSQ, catToTsType, getVarsFromCats, mkTokenNodeName, indent, getAllTokenCats, getAllTokenTypenames)
import BNFC.Options (SharedOptions (lang))
import BNFC.Backend.Antlr.CFtoAntlr4Parser (antlrRuleLabel, makeLeftRecRule)
import BNFC.Backend.Common.NamedVariables (firstUpperCase)

type RuleData = (Cat, [(String, SentForm)])

cfToBuilder :: CF -> SharedOptions -> Doc
cfToBuilder cf opts = vcat $ intersperse (text "")
    [ importDecls
    , tokenDecls
    , buildFnDecls
    ]
  where
    importDecls = mkImportDecls cf (lang opts)

    tokenDecls = vcat $ intersperse (text "") buildTokensFuns
    buildFnDecls = vcat $ intersperse (text "") buildFuns

    buildFuns = map mkBuildFunction datas
    buildTokensFuns = map mkBuildTokenFunction allTokenCats

    allTokenCats = getAllTokenCats cf
    datas = cfToGroups cf

mkThrowErrorStmt :: Cat -> String
mkThrowErrorStmt cat = "throw new Error('[" ++ mkBuildFnName cat ++ "]" +++ "Error: arg should be an instance of" +++ camelCase_ (identCat cat) ++ "Context" ++ "')"

-- | generates function code for building appropriate node for TokenCat.
mkBuildTokenFunction :: Cat -> Doc
mkBuildTokenFunction tokenCat = vcat
    [ text $ "export function" +++ fnName ++ "(arg: Token):" +++ returnType +++ "{"
    , indent 2 "return {"
    , indent 4 $ "type:" +++ mkTokenNodeName tokenName ++ ","
    , indent 4 $ "value:" +++ value
    , indent 2 "}"
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

-- | generate name for function which will build node for some cat.
mkBuildFnName :: Cat -> String
mkBuildFnName cat = "build" ++ firstUpperCase (restName cat)
  where
    restName cat = case cat of
      ListCat cat  -> restName cat ++ "List"
      TokenCat cat -> cat ++ "Token"
      otherCat     -> catToStr otherCat

-- | generates import declarations for antlr nodes and AST nodes.
mkImportDecls :: CF -> String -> Doc
mkImportDecls cf lang = vcat
    [ "import {Token} from 'antlr4'"
    , text ctxImportStmt
    , text astImportStmt
    ]
  where
    groups = cfToGroups cf
    ctxNames = concatMap (\(cat, rules) -> identCat cat : zipWith (\(fun, _) idx -> antlrRuleLabel cat fun (Just idx)) rules [1..]) groups
    ctxImports = intercalate ", " $ nub $ map (++ "Context") ctxNames

    tokenTypenames = getAllTokenTypenames cf
    typenames = nub $ tokenTypenames ++ (map (catToTsType . fst) $ filter (isUsualCat . fst) groups)
    astImports = intercalate ", " (filter (not . null) typenames)

    ctxImportStmt = "import {" ++ ctxImports ++ "} from './" ++ parserFile ++ "'"
    astImportStmt = "import {" ++ astImports ++ "} from './abstract'"

    parserFile = camelCase_ $ lang ++ "Parser"

    isUsualCat (Cat _) = True
    isUsualCat _       = False

mkBuildFunction :: RuleData -> Doc
mkBuildFunction (cat, rulesWithLabels) = vcat
    [ text $ "export function" +++ mkBuildFnName cat ++ "(arg: " ++ identCat cat ++ "Context):" +++ catToTsType cat +++ "{"
    , vcat $ map mkIfStmt datas
    , indent 2 $ mkThrowErrorStmt cat
    , "}"
    ]
  where
    datas = zip rulesWithLabels [1..]

    mkIfStmt :: ((String, SentForm), Integer) -> Doc
    mkIfStmt ((ruleLabel, rhsRule), ifIdx) = vcat
        [ indent 2 $ "if (arg instanceof" +++ antlrRuleLabel cat ruleLabel antlrRuleLabelIdx ++ "Context) {"
        , vcat $ map text $ mkIfBody ruleLabel
        , indent 2 "}"
        ]

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
