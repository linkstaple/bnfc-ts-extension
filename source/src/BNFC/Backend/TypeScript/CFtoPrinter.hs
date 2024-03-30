module BNFC.Backend.TypeScript.CFtoPrinter (cfToPrinter) where

import Data.Either (lefts, rights)
import Data.List (nub, intercalate, find, uncons, intersperse)

import Text.PrettyPrint.HughesPJClass (Doc, text, vcat, nest)

import BNFC.CF (CF, ruleGroups, Rul (rhsRule, funRule), Cat (Cat, ListCat, TokenCat), WithPosition (wpThing), IsFun (isCoercion, isConsFun, isOneFun, isNilFun), catToStr, SentForm, rulesForNormalizedCat, normCat, getAbstractSyntax, normCatOfList, catOfList, isList)
import BNFC.Utils ((+++))
import BNFC.Backend.TypeScript.Utils (catToTsType, indent, wrapSQ, getVarsFromCats, toMixedCase, getAbsynWithoutLists, getAllTokenTypenames, getAllTokenCats)
import BNFC.Backend.Common.NamedVariables (firstUpperCase)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- | generate pretty-printers for nodes of an AST
cfToPrinter :: CF -> Doc
cfToPrinter cf = vcat
    [ importsDecl
    , rendererDeclaration
    , tokenPrinterDecl
    , nodesPrintersDecls
    , nodesPrettifiersDecls
    , rulesPrintersDecls
    ]
  where
    importsDecl = mkImportDecls cf
    tokenPrinterDecl = mkTokenPrinter cf

    cats = map fst $ getAbstractSyntax cf
    nodesPrettifiersDecls = vcat $ intersperse (text "") $ map (mkNodePrettifier cf) cats
    nodesPrintersDecls = vcat $ intersperse (text "") $ map mkNodePrinter cats

    rulesPrintersDecls = vcat $ map mkRulePrinter rules
    rules = map (wpThing . funRule) $
              concatMap
              (filter (not . isCoercion) . rulesForNormalizedCat cf)
              (filter (not . isList) cats)

rendererDeclaration :: Doc
rendererDeclaration = vcat
  [ "class Token {}"
  , "class TextToken extends Token {"
  , nest 2 $ vcat
    [ "constructor(public text: string) {"
    , nest 2 "super()"
    , "}"
    , "toString() {"
    , nest 2 "return this.text"
    , "}"
    ]
  , "}"

  , "class SpaceToken extends TextToken {"
  , nest 2 $ vcat
    [ "constructor() {"
    , nest 2 "super(' ')"
    , "}"
    , "toString() {"
    , nest 2 "return ' '"
    , "}"
    ]
  , "}"

  , "class NewLineToken extends Token {"
  , nest 2 $ vcat
    [ "indentShift = 0"
    , "constructor() {"
    , nest 2 "super()"
    , "}"
    , "nest() {"
    , nest 2 "this.indentShift = 1"
    , "}"
    , "unnest() {"
    , nest 2 "this.indentShift = -1"
    , "}"
    , "toString() {"
    , nest 2 "return '\\n'"
    , "}"
    ]
  , "}"
  , "class StringRenderer {"
  , nest 2 $ vcat
    [ "private indentSize = 2"
    , "print(tokens: string[]): string {"
    , nest 2 "return this.render(tokens)"
    , nest 2 $ vcat
      [ ".reduce(this.split.bind(this), [])"
      , ".map(this.addIndentation.bind(this))"
      , ".map(tokens => tokens.map(token => token.toString()).join(''))"
      , ".join('\\n')"
      ]
    , "}"
    , "render(tokens: string[]): Token[] {"
    , nest 2 $ vcat
      [ "const tokensList = tokens.reduce<Token[]>((tokensList, token) => {"
      , nest 2 $ vcat
        [ "if (['', ' '].includes(token)) {"
        , nest 2 "return tokensList"
        , "}"
        , "if (token === '{') {"
        , nest 2 "const newLine = new NewLineToken()"
        , nest 2 "newLine.nest()"
        , nest 2 "tokensList.push(new TextToken(token), newLine)"
        , "} else if (token === '}') {"
        , nest 2 "this.dropTrailingNewLines(tokensList)"
        , nest 2 "const newLine = new NewLineToken()"
        , nest 2 "newLine.unnest()"
        , nest 2 "tokensList.push(newLine, new TextToken(token), new NewLineToken())"
        , "} else if (['!', '@', '&', '<', '[', '.', '$', '#'].includes(token)) {"
        , nest 2 "tokensList.push(new TextToken(token))"
        , "} else if (token === '(') {"
        , nest 2 "this.dropTrailingSpaces(tokensList)"
        , nest 2 "tokensList.push(new TextToken(token))"
        , "} else if ([')', ']', '>'].includes(token)) {"
        , nest 2 "this.dropTrailingSpaces(tokensList)"
        , nest 2 "this.dropTrailingNewLines(tokensList)"
        , nest 2 "tokensList.push(new TextToken(token), new SpaceToken())"
        , "} else if (token === ';') {"
        , nest 2 "tokensList.push(new TextToken(token), new NewLineToken())"
        , "} else {"
        , nest 2 "tokensList.push(new TextToken(token), new SpaceToken())"
        , "}"
        , "return tokensList"
        ]
      , "}, [])"
      , "this.dropTrailingSpaces(tokensList)"
      , "this.dropTrailingNewLines(tokensList)"
      , "return tokensList"
      ]
    , "}"
    , "split(splitData: Array<[number, Token[]]>, token: Token): Array<[number, Token[]]> {"
    , nest 2 $ vcat
      [ "if (token instanceof NewLineToken) {"
      , nest 2 "const indentationLevel = splitData[splitData.length - 1]?.[0] ?? 0"
      , nest 2 "splitData.push([indentationLevel + token.indentShift, []])"
      , "} else if (splitData.length == 0) {"
      , nest 2 "splitData.push([0, [token]])"
      , "} else {"
      , nest 2 "splitData[splitData.length - 1][1].push(token)"
      , "}"
      , "return splitData"
      ]
    , "}"
    , "addIndentation([indentationLevel, tokens]: [number, Token[]]): Token[] {"
    , nest 2 $ vcat
      [ "if (indentationLevel === 0) {"
      , nest 2 "return tokens"
      , "}"
      , "tokens.unshift(new TextToken(' '.repeat(this.indentSize * indentationLevel)))"
      , "return tokens"
      ]
    , "}"
    , "private dropTrailingSpaces(list: Token[]) {"
    , nest 2 "while (list.length > 0 && list[list.length - 1] instanceof SpaceToken) {"
    , nest 4 "list.pop()"
    , nest 2 "}"
    , "}"
    , "private dropTrailingNewLines(list: Token[]) {"
    , nest 2 "while (list.length > 0 && list[list.length - 1] instanceof NewLineToken) {"
    , nest 4 "list.pop()"
    , nest 2 "}"
    , "}"
    ]
  ,"}"
  ,""
  , "const renderer = new StringRenderer()"
  ]

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

mkNodePrinter :: Cat -> Doc
mkNodePrinter cat@(Cat _) = vcat
    [ text $ "export function " ++ printFnName ++ "(node: " ++ catToTsType cat ++ "): string {"
    , indent 2 $ "return renderer.print(" ++ prettifyFnName ++ "(node))"
    , "}"
    ]
  where
    printFnName    = mkPrintFnName cat
    prettifyFnName = mkPrettifyFnName cat

mkNodePrinter listCat@(ListCat _) = vcat
    [ text $ "export function " ++ printFnName ++ "(list: Array<" ++ catOfListType ++ ">): string {"
      , indent 2 $ "return renderer.print(" ++ prettifyFnName ++ "(list))"
      , "}"
    ]
  where
    prettifyFnName = mkPrettifyFnName listCat
    printFnName    = mkPrintFnName listCat
    catOfListType  = catToTsType (normCatOfList listCat)

mkNodePrinter otherCat = error $ "Unknown category for making node printer" +++ catToStr otherCat

mkRulePrinter :: String -> Doc
mkRulePrinter ruleLabel = vcat
    [ text $ "export function" +++ printFnName ++ "(node:" +++ toMixedCase ruleLabel ++ "): string {"
    , indent 2 $ "return renderer.print(" ++ prettifyFnName ++ "(node))"
    , "}"
    ]
  where
    printFnName    = "print" ++ firstUpperCase ruleLabel
    prettifyFnName = "prettify" ++ firstUpperCase ruleLabel

mkNodePrettifier :: CF -> Cat -> Doc
mkNodePrettifier cf cat@(Cat _) = vcat $ concat
    [ [text $ "function" +++ prettifyFnName ++ "(node:" +++ catToTsType cat ++ "): string[] {" ]
    , prettifyRulesCondition
    , [indent 2 $ "throw new Error(`[" ++ prettifyFnName ++ "]: Unkown node`)"]
    , ["}"]
    , [""]
    , rulesPrettifiers
    ]
  where      
    rules = map (\rule -> (wpThing (funRule rule), rhsRule rule)) $
              filter (not . isCoercion . funRule) $
              rulesForNormalizedCat cf cat
    
    mkIfStmt label = vcat
        [ indent 2 $ "if (node.type === " ++ wrapSQ label ++ ") {"
        , indent 4 $ "return prettify" ++ firstUpperCase label ++ "(node)"
        , indent 2 "}"
        ]

    labels = map fst rules

    prettifyRulesCondition = map mkIfStmt labels
    prettifyFnName = mkPrettifyFnName cat
    rulesPrettifiers = map mkRulePrettifier rules

mkNodePrettifier cf listCat@(ListCat _) = vcat
    [ text $ "function " ++ prettifyFnName ++ "(list: Array<" ++ catOfListType ++ ">): string[] {"
    , nest 2 returnStmt
    , "}"
    ]
  where
    prettifyFnName = mkPrettifyFnName listCat
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

    separator = fromMaybe "" consSeparator
    isTerminator = (isJust nilRule && isNothing oneRule && isJust consRule && isJust consSeparator)
      || (isNothing nilRule && isJust oneRule && isJust oneSeparator && isJust consRule && isJust consSeparator)

    itemCat = catOfList listCat
    printItemFn tokenCat@(TokenCat _) = mkPrintFnName tokenCat
    printItemFn cat                  = "..." ++ mkPrettifyFnName cat

    returnStmt = vcat
        [ "return list.length === 0"
        , nest 2 "? []"
        , indent 2 $ ": " ++ listTokens
        ]
      where
        listMapping = "list.map(item => [" ++ printItemFn itemCat ++ "(item), " ++ wrapSQ separator ++ "]).flat()"
        listTokens = 
          listMapping ++ if isTerminator then "" else ".slice(0, -1)"

mkNodePrettifier _ otherCat = error $ "Unknown category for making node prettifier" +++ catToStr otherCat

mkRulePrettifier :: (String, SentForm) -> Doc
mkRulePrettifier (ruleLabel, sentForm) = vcat
    [ text $ "function" +++ prettifyFnName ++ "(node:" +++ toMixedCase ruleLabel ++ "): string[] {"
    , indent 2 prettifyBody
    , "}"
    ]
  where
    varNames = getVarsFromCats (lefts sentForm)

    addVarNames :: [Either Cat String] -> [String] -> [Either (Cat, String) String]
    addVarNames [] _                      = []
    addVarNames list []                   = map (either (\cat -> Left (cat, "")) Right) list
    addVarNames (x:xs) allVars@(var:vars) = case x of
      (Right terminal)  -> Right terminal : addVarNames xs allVars
      (Left cat)        -> Left (cat, var) : addVarNames xs vars
    
    sentFormWithVarNames = addVarNames sentForm varNames
    prettifiedRule = intercalate ", " $ map (either getPrettifierForCat wrapSQ) sentFormWithVarNames
      where
        getPrettifierForCat :: (Cat, String) -> String
        getPrettifierForCat (tokenCat@(TokenCat _), varName) = mkPrintFnName tokenCat ++ "(node." ++ varName ++ ")"
        getPrettifierForCat (cat, varName)                   = "..." ++ mkPrettifyFnName cat ++ "(node." ++ varName ++ ")"

    prettifyFnName = "prettify" ++ firstUpperCase ruleLabel
    prettifyBody = "return [" ++ prettifiedRule ++ "]"

mkPrettifyFnName :: Cat -> String
mkPrettifyFnName cat = "prettify" ++ mkName normalizedCat
  where
    mkName (ListCat cat) = ("ListOf"++) $ firstUpperCase (mkName cat)
    mkName otherCat      = firstUpperCase $ catToStr otherCat
    normalizedCat = normCat cat

mkPrintFnName :: Cat -> String
mkPrintFnName cat = "print" ++ mkName normalizedCat
  where
    mkName (ListCat cat) = ("ListOf"++) $ firstUpperCase (mkName cat)
    mkName otherCat      = firstUpperCase $ catToStr otherCat
    normalizedCat = normCat cat
