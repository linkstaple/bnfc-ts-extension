module BNFC.Backend.TypeScript.CFtoBuilder where

import Text.PrettyPrint.HughesPJClass (Doc, text, vcat)
import Data.Bifunctor (Bifunctor(second))
import Data.List (intercalate)

import BNFC.Utils ((+++), camelCase_, mixedCase_)
import BNFC.CF (CF, Cat (ListCat, TokenCat), identCat, isList, IsFun (isNilFun, isOneFun, isConsFun), getAbstractSyntax, Data, catOfList, isTokenCat, catToStr)
import BNFC.Backend.TypeScript.Utils (indentStr, wrapSQ, withOccurences, getTokenCats, catToTsType)
import BNFC.Options (SharedOptions (lang))
import BNFC.Backend.Antlr.CFtoAntlr4Parser (catToNT)

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
      buildFnDecls = map text $ intercalate [""] (map mkBuildFunction datas)

      importDecls = map text $ mkImportDecls datas (lang opts)

      tokenDecls = map text $ intercalate [""] $ map mkBuildTokenFunction allTokenCats
      allTokenCats = getTokenCats datas

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

    mkConsListCheck cat =
        [ "if (arg instanceof" +++ catid ++ "_PrependFirstContext) {"
        , indentStr 2 $ "const item = arg." ++ itemName ++ "()"
        , indentStr 2 $ "const list = arg." ++ listName ++ "()"
        , indentStr 2 $ "const data =" +++ buildItemFn ++ "(item)"
        , indentStr 2 $ "return [data].concat(" ++ fnName ++ "(" ++ "list" ++ ")"++ ")"
        , "}"
        ]
      where
        itemName = mixedCase_ (show cat)
        buildItemFn = mkBuildFnName cat
        listName = mixedCase_ catid

    mkOneListCheck cat =
        [ "if (arg instanceof" +++ catid ++ "_AppendLastContext) {"
        , indentStr 2 $ "const item = arg." ++ itemName ++ "()"
        , indentStr 2 $ "const data =" +++ buildItemFn ++ "(item)"
        , indentStr 2 "return [data]"
        , "}"
        ]
      where
        itemName = mixedCase_ (show cat)
        buildItemFn = mkBuildFnName cat

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

mkImportDecls :: [Data] -> String -> [String]
mkImportDecls datas lang = [ctxImportStmt, astImportStmt]
  where
    contextImports = intercalate ", " $ map (mkCtxType . excludeValues) datas

    tokenTypes = map catToTsType (getTokenCats datas)
    nonListCats = map fst $ filter (not . isList . fst) datas
    catTypes = map catToTsType nonListCats

    catsTypeNames = tokenTypes ++ catTypes
    astImports = intercalate ", " catsTypeNames

    ctxImportStmt = "import {" ++ contextImports ++ "} from './" ++ parserFile ++ "'"
    astImportStmt = "import {" ++ astImports ++ "} from './abstract'"

    parserFile = camelCase_ $ lang ++ "Parser"

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
