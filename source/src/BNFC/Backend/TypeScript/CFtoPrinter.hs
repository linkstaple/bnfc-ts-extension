module BNFC.Backend.TypeScript.CFtoPrinter (cfToPrinter) where
import BNFC.CF (CF, getAbstractSyntax, ruleGroups, Rul (rhsRule, funRule), Cat (Cat, TokenCat), identCat, literals, allParserCats, rulesForCat, WithPosition (wpThing), isList, allParserCatsNorm, IsFun (isCoercion), catToStr)
import Text.PrettyPrint.HughesPJClass (Doc, text, vcat)
import Data.Bifunctor (second)
import BNFC.Utils ((+++), camelCase_)
import BNFC.Backend.TypeScript.Utils (catToTsType, indent, indentStr, wrapSQ, mkTokenNodeName)
import Data.List (nub, intercalate)
import BNFC.Backend.Common.NamedVariables (firstUpperCase)

cfToPrinter :: CF -> Doc
cfToPrinter cf = vcat
  [ importsDecl
  , tokenPrinterDecl
  , nodesPrinterDecl
  , rulesPrintersDecls
  ]
    where
      importsDecl = text $ mkImportDecls cf
      tokenPrinterDecl = mkTokenPrinter cf
      nodesPrinterDecl = mkNodesPrinter cf

      rulesPrintersDecls = vcat $ map (mkNodePrinter cf) cats

      cats = filter (not . isList) $ map fst $ getAbstractSyntax cf


mkImportDecls :: CF -> String
mkImportDecls cf = astImportStmt
  where
    rulesCats = map fst (ruleGroups cf)

    tokenImports = map (catToTsType . TokenCat) (literals cf)
    catsImports = map catToTsType $ filter isUsualCat rulesCats

    astNames = nub $ tokenImports ++ catsImports
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
  [ text $ "export function printNode(node: " ++ catsUnionType ++ "): string {"
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
        , indent 4 "// return node.map(printNode).join(',')"
        , indent 4 "return ''"
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
  , [indent 2 $ "throw new Error('[" ++ mkPrintFnName cat ++ "]: Unkown node')"]
  , ["}"]
  ]
    where
      rules = map (\rule -> (wpThing (funRule rule), rhsRule rule)) $ rulesForCat cf cat
      filteredRules = filter (not . isCoercion . fst) rules

      rulesConditions = map mkIfStmt filteredRules
      printFnName = mkPrintFnName cat

      mkIfStmt (label, _sentForm) = vcat
        [ indent 2 $ "if (node.type === " ++ wrapSQ label ++ ") {"
        , indent 4 "return ''"
        , indent 2 "}"
        ]




mkRulePrinter :: CF -> Doc
mkRulePrinter cf = text ""


mkPrintFnName :: Cat -> String
mkPrintFnName cat = "print" ++ firstUpperCase (catToStr cat)

