module BNFC.Backend.TypeScript.CFtoPrinter (cfToPrinter) where

import Data.Either (lefts, rights)
import Data.List (nub, intercalate, find, uncons)

import Text.PrettyPrint.HughesPJClass (Doc, text, vcat, nest)

import BNFC.CF (CF, ruleGroups, Rul (rhsRule, funRule), Cat (Cat, ListCat), WithPosition (wpThing), IsFun (isCoercion, isConsFun, isOneFun), catToStr, SentForm, rulesForNormalizedCat, normCat, getAbstractSyntax, normCatOfList)
import BNFC.Utils ((+++))
import BNFC.Backend.TypeScript.Utils (catToTsType, indent, wrapSQ, getVarsFromCats, toMixedCase, getAbsynWithoutLists, getAllTokenTypenames, getAllTokenCats)
import BNFC.Backend.Common.NamedVariables (firstUpperCase)
import Data.Maybe (isJust, isNothing)

cfToPrinter :: CF -> Doc
cfToPrinter cf = vcat
    [ importsDecl
    , tokenPrinterDecl
    , nodePrinterDecls
    ]
  where
    importsDecl = mkImportDecls cf
    tokenPrinterDecl = mkTokenPrinter cf

    nodePrinterDecls = vcat $ map (mkNodePrinter cf) cats
    cats = map fst $ getAbstractSyntax cf

mkImportDecls :: CF -> Doc
mkImportDecls cf = text astImportStmt
  where
    rulesCats = map fst (ruleGroups cf)

    tokensTypeNames = getAllTokenTypenames cf
    catsTypeNames = map catToTsType $ filter isUsualCat rulesCats

    labelsTypeNames = concatMap (map (toMixedCase . fst) . snd) $
                          getAbsynWithoutLists cf

    allTypeNames = nub $ tokensTypeNames ++ catsTypeNames ++ labelsTypeNames
    astImports = intercalate ", " (filter (not . null) allTypeNames)

    astImportStmt = "import {" ++ astImports ++ "} from './abstract'"

    isUsualCat (Cat _) = True
    isUsualCat _       = False

-- | generate function which will print user-defined and predefined tokens.
mkTokenPrinter :: CF -> Doc
mkTokenPrinter cf = vcat
    [ text $ "export function printToken(token: " ++ tokensUnionType ++ "): string {"
    , indent 2 "return String(token.value)"
    , "}"
    , tokenPrinters
    ]
  where
    allTokenTypes = getAllTokenTypenames cf
    tokensUnionType = intercalate " | " allTokenTypes

    tokenPrinters = vcat $ map mkTokenPrinter (getAllTokenCats cf)
    mkTokenPrinter tokenCat = vcat
      [ text $ "export function" +++ mkPrintFnName tokenCat ++ "(token: " ++ catToTsType tokenCat ++ "): string {"
      , indent 2 "return String(token.value)"
      , "}"
      ]

mkNodePrinter :: CF -> Cat -> Doc
mkNodePrinter cf cat@(Cat _) = vcat $ concat
    [ [text $ "export function" +++ printFnName ++ "(node:" +++ catToTsType cat ++ "): string {" ]
    , rulesConditions
    , [indent 2 $ "throw new Error(`[" ++ printFnName ++ "]: Unkown node`)"]
    , ["}"]
    , rulesPrinters
    ]
  where      
    rules = map (\rule -> (wpThing (funRule rule), rhsRule rule)) $
              filter (not . isCoercion . funRule) $
              rulesForNormalizedCat cf cat

    labels = map fst rules
    rulesConditions = map mkIfStmt labels
    rulesPrinters = map mkRulePrinter rules

    mkIfStmt label = vcat
      [ indent 2 $ "if (node.type === " ++ wrapSQ label ++ ") {"
      , indent 4 $ "return print" ++ firstUpperCase label ++ "(node)"
      , indent 2 "}"
      ]

    printFnName = mkPrintFnName cat

mkNodePrinter cf listCat@(ListCat _) = vcat
    [ text $ "export function " ++ fnName ++ "(list: Array<" ++ catOfListType ++ ">): string {"
    , nest 2 returnStmt
    , "}"
    ]
  where
    fnName = mkPrintFnName listCat
    catOfListType = catToTsType (normCatOfList listCat)

    rules = rulesForNormalizedCat cf listCat
    consRule = find (isConsFun . funRule) rules
    consSeparator = maybe Nothing findSeparator consRule

    oneRule = find (isOneFun . funRule) rules
    oneSeparator = maybe Nothing findSeparator oneRule

    findSeparator :: Rul a -> Maybe String
    findSeparator rule = fmap fst (uncons terminals)
      where
        terminals = rights (rhsRule rule)

    separator = maybe " " (++ " ") consSeparator
    isSeparator = isJust consSeparator && isJust oneRule && isNothing oneSeparator

    printItemFn = mkPrintFnName (normCatOfList listCat)
    returnStmt = if isSeparator
      then text $ "return list.map(" ++ printItemFn ++ ").join('" ++ separator ++ "')"
      else vcat
        [ "if (list.length === 0) {"
        , indent 2 "return ''"
        , "}"
        , text $ "return list.map(" ++ printItemFn ++ ").join('" ++ separator ++ "') + '" ++ separator ++ "'"
        ]

mkNodePrinter _ otherCat = error $ "Unknown category for making node printer" +++ catToStr otherCat

mkRulePrinter :: (String, SentForm) -> Doc
mkRulePrinter (ruleLabel, sentForm) = vcat
    [ text $ "export function" +++ fnName ++ "(node:" +++ toMixedCase ruleLabel ++ "): string {"
    , indent 2 $ "return `" ++ prettifiedRule ++ "`"
    , "}"
    ]
  where
    fnName = "print" ++ firstUpperCase ruleLabel

    -- [Left True, Right "Hi", Left False] -> [Left (True, 0), Right "Hi", Left (False, 1)]
    leftsIndexed :: [Either a b] -> Int -> [Either (a, Int) b]
    leftsIndexed [] _ = []
    leftsIndexed ((Right item) : others) currentIdx = Right item : leftsIndexed others currentIdx
    leftsIndexed ((Left item) : others) currentIdx = Left (item, currentIdx) : leftsIndexed others (currentIdx + 1)

    varNames = getVarsFromCats (lefts sentForm)
    sentFormWithVarNames = map (either (\(cat, idx) ->
      "${" ++ mkPrintFnName (normCat cat) ++ "(node." ++ varNames !! idx ++ ")}") id) $ leftsIndexed sentForm 0

    prettifiedRule = unwords sentFormWithVarNames

mkPrintFnName :: Cat -> String
mkPrintFnName cat = "print" ++ firstUpperCase (mkName cat)
  where
    mkName (ListCat cat) = mkName cat ++ "List"
    mkName otherCat      = catToStr otherCat
