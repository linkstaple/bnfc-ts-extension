module BNFC.Backend.TypeScript.CFtoPrinter (cfToPrinter) where

import Data.Either (lefts, rights)
import Data.List (nub, intercalate, find, uncons)

import Text.PrettyPrint.HughesPJClass (Doc, text, vcat, nest)

import BNFC.CF (CF, ruleGroups, Rul (rhsRule, funRule), Cat (Cat, ListCat), WithPosition (wpThing), IsFun (isCoercion, isConsFun, isOneFun, isNilFun), catToStr, SentForm, rulesForNormalizedCat, normCat, getAbstractSyntax, normCatOfList, catOfList)
import BNFC.Utils ((+++))
import BNFC.Backend.TypeScript.Utils (catToTsType, indent, wrapSQ, getVarsFromCats, toMixedCase, getAbsynWithoutLists, getAllTokenTypenames, getAllTokenCats)
import BNFC.Backend.Common.NamedVariables (firstUpperCase)
import Data.Maybe (isJust, isNothing)

cfToPrinter :: CF -> Doc
cfToPrinter cf = vcat
    [ importsDecl
    -- , listPrinterDecl
    , tokenPrinterDecl
    , nodePrinterDecls
    ]
  where
    importsDecl = mkImportDecls cf
    tokenPrinterDecl = mkTokenPrinter cf

    nodePrinterDecls = vcat $ map (mkNodePrinter cf) cats
    cats = map fst $ getAbstractSyntax cf

    -- listPrinterDecl = vcat
    --   [ "export function printList_<T>(list: Array<T>, printCb: (item: T) => string, sep: string, isTerminator: boolean): string {"
    --   , indent 2 $ "if (list.length === 0) {"
    --   , indent 4 $ "return ''"
    --   , indent 2 $ "}"
    --   , indent 2 $ "const result = list.map(printCb).join(sep)"
    --   , indent 2 $ "const tail = isTerminator ? sep : ''"
    --   , indent 2 $ "return result + tail"
    --   , "}"
    --   ]

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

-- | get separator for list of cats and a flag if it is actually terminator
-- true - separator, false - terminator
-- getSepForListCat :: CF -> Cat -> (String, Bool)
-- getSepForListCat cf listCat@(ListCat _) = (separator, not isTerminator)
--   where
--     rules = rulesForNormalizedCat cf listCat
--     consRule = find (isConsFun . funRule) rules
--     consSeparator = maybe Nothing findSeparator consRule

--     oneRule = find (isOneFun . funRule) rules
--     oneSeparator = maybe Nothing findSeparator oneRule

--     nilRule = find (isOneFun . funRule) rules

--     findSeparator :: Rul a -> Maybe String
--     findSeparator rule = fmap fst (uncons terminals)
--       where
--         terminals = rights (rhsRule rule)

--     separator = maybe " " (++ " ") consSeparator
--     -- isSeparator = isJust consSeparator && isJust oneRule && isNothing oneSeparator
--     isTerminator = (isJust nilRule && isNothing oneRule && isJust consRule && isJust consSeparator)
--       || (isNothing nilRule && isJust oneRule && isJust oneSeparator && isJust consRule && isJust consSeparator)

-- getSepForListCat _ cat = error $ "'getSepForListCat' can not be applied to non-list cat " ++ show cat

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
    rulesPrinters = map (mkRulePrinter cf) rules

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

    nilRule = find (isNilFun . funRule) rules

    findSeparator :: Rul a -> Maybe String
    findSeparator rule = fmap fst (uncons terminals)
      where
        terminals = rights (rhsRule rule)

    separator = maybe " " (++ " ") consSeparator
    isTerminator = (isJust nilRule && isNothing oneRule && isJust consRule && isJust consSeparator)
      || (isNothing nilRule && isJust oneRule && isJust oneSeparator && isJust consRule && isJust consSeparator)

    printItemFn = mkPrintFnName (catOfList listCat)
    returnStmt = if not isTerminator
      then text $ "return list.map(" ++ printItemFn ++ ").join('" ++ separator ++ "')"
      else vcat
        [ "if (list.length === 0) {"
        , indent 2 "return ''"
        , "}"
        , text $ "return list.map(" ++ printItemFn ++ ").join('" ++ separator ++ "') + '" ++ separator ++ "'"
        ]

mkNodePrinter _ otherCat = error $ "Unknown category for making node printer" +++ catToStr otherCat

mkRulePrinter :: CF -> (String, SentForm) -> Doc
mkRulePrinter cf (ruleLabel, sentForm) = vcat
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
      "${" ++ getPrinterForCat (cat, idx) ++ "}") id) $ leftsIndexed sentForm 0

    getPrinterForCat :: (Cat, Int) -> String
    -- getPrinterForCat (listCat@(ListCat itemCat), idx) = "printList_(" ++ listArgument ++ "," +++ printCbArgument ++ ", '" ++ sep ++ "', " ++ isTerminalArgument ++ ")"
    --   where
    --     (sep, isSep) = getSepForListCat cf listCat

    --     listArgument = "node." ++ varNames !! idx
    --     isTerminalArgument = if isSep then "false" else "true"
    --     printCbArgument = mkPrintFnName itemCat

    getPrinterForCat (cat, idx) = mkPrintFnName cat ++ "(node." ++ varNames !! idx ++ ")"

    prettifiedRule = unwords sentFormWithVarNames

mkPrintFnName :: Cat -> String
mkPrintFnName cat = "print" ++ mkName normalizedCat
  where
    mkName (ListCat cat) = ("ListOf"++) $ firstUpperCase (mkName cat)
    mkName otherCat      = firstUpperCase $ catToStr otherCat
    normalizedCat = normCat cat
