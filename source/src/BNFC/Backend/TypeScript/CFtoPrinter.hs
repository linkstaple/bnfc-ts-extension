module BNFC.Backend.TypeScript.CFtoPrinter (cfToPrinter) where
import BNFC.CF (CF, getAbstractSyntax, ruleGroups, Rul (rhsRule, funRule), Cat (Cat, TokenCat), literals, WithPosition (wpThing), isList, allParserCatsNorm, IsFun (isCoercion), catToStr, SentForm, rulesForNormalizedCat)
import Text.PrettyPrint.HughesPJClass (Doc, text, vcat)
import BNFC.Utils ((+++))
import BNFC.Backend.TypeScript.Utils (catToTsType, indent, wrapSQ, mkTokenNodeName, getVarsFromCats, toMixedCase)
import Data.List (nub, intercalate)
import BNFC.Backend.Common.NamedVariables (firstUpperCase)
import Data.Either (lefts)

cfToPrinter :: CF -> Doc
cfToPrinter cf = vcat
  [ importsDecl
  , tokenPrinterDecl
  , treePrinterDecl
  , nodePrinterDecls
  ]
    where
      importsDecl = mkImportDecls cf
      tokenPrinterDecl = mkTokenPrinter cf
      treePrinterDecl = mkNodesPrinter cf

      nodePrinterDecls = vcat $ map (mkNodePrinter cf) cats

      cats = filter (not . isList) $
              map fst (getAbstractSyntax cf)

mkImportDecls :: CF -> Doc
mkImportDecls cf = text astImportStmt
  where
    rulesCats = map fst (ruleGroups cf)

    tokenImports = map (catToTsType . TokenCat) (literals cf)
    catsImports = map catToTsType $ filter isUsualCat rulesCats

    rulesLabelsImports = concatMap (map (toMixedCase . fst) . snd) $
              filter (not . isList . fst) $ getAbstractSyntax cf

    astNames = nub $ tokenImports ++ catsImports ++ rulesLabelsImports
    astImports = intercalate ", " (filter (not . null) astNames)

    astImportStmt = "import {" ++ astImports ++ "} from './abstract'"

    isUsualCat (Cat _) = True
    isUsualCat _       = False

mkTokenPrinter :: CF -> Doc
mkTokenPrinter cf = vcat
  [ text $ "export function printToken(token: " ++ tokensUnionType ++ "): string {"
  , indent 2 "return String(token.value)"
  , "}"
  ]
    where
      allTokenTypes = map (catToTsType . TokenCat) $ literals cf
      tokensUnionType = intercalate " | " allTokenTypes

mkNodesPrinter :: CF -> Doc
mkNodesPrinter cf = vcat $
  [ text $ "export function printTree(node: " ++ catsUnionType ++ "): string {"
  , arrayCheck
  , tokenCheck
  ] ++
  nodeConditions ++
  [ indent 2 "console.error(node)"
  , indent 2 "throw new Error('Unknown node')"
  , text "}"]
    where
      allCats = allParserCatsNorm cf
      catsTypes = map catToTsType allCats
      allTokenCats = map (catToTsType . TokenCat) $ literals cf
      catsUnionType = intercalate " | " $ allTokenCats ++ catsTypes

      absynWithoutLists = filter (not . isList . fst) $ getAbstractSyntax cf

      nodeConditions = map mkNodeCondition absynWithoutLists

      mkNodeCondition (cat, rhs) = vcat
        [ indent 2 $ "if (" ++ joinedLabels ++ ") {"
        , indent 4 $ "return " ++ mkPrintFnName cat ++ "(node)"
        , indent 2 "}"
        ]
          where
            labels = map (wrapSQ . fst) rhs
            joinedLabels = intercalate " || " $ map ("node.type === " ++) labels

      arrayCheck :: Doc = vcat
        [ indent 2 "if (Array.isArray(node)) {"
        , indent 4 "return node.map(printTree).join(', ')"
        , indent 2 "}"
        ]

      allTokens = literals cf
      tokenComparisons = map (("node.type === " ++) . mkTokenNodeName) allTokens
      tokenCondition = intercalate " || " tokenComparisons
      tokenCheck :: Doc = vcat
        [ indent 2 $ "if (" ++ tokenCondition ++ ") {"
        , indent 4 "return printToken(node)"
        , indent 2 "}" 
        ]

mkNodePrinter :: CF -> Cat -> Doc
mkNodePrinter cf cat = vcat $ concat
  [ [text $ "export function" +++ printFnName ++ "(node:" +++ catToTsType cat ++ "): string {" ]
  , rulesConditions
  , [indent 2 $ "throw new Error(`[" ++ mkPrintFnName cat ++ "]: Unkown node`)"]
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
      sentFormWithVarNames = map (either (\(_, idx) -> "${printTree(node." ++ varNames !! idx ++ ")}") id) $ leftsIndexed sentForm 0

      prettifiedRule = unwords sentFormWithVarNames

mkPrintFnName :: Cat -> String
mkPrintFnName cat = "print" ++ firstUpperCase (catToStr cat)

