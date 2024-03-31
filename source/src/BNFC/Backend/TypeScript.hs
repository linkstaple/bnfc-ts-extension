{-# LANGUAGE RecordWildCards #-}

module BNFC.Backend.TypeScript ( makeTypeScript ) where

import Text.PrettyPrint ( text, vcat, render, nest )

import Prelude hiding ((<>))
import System.FilePath ((</>), pathSeparator)
import System.Directory ( createDirectoryIfMissing )
import Data.Char (toLower)

import BNFC.Backend.Base (MkFiles, mkfile,liftIO)
import BNFC.CF (CF, getAbstractSyntax)
import BNFC.Options (SharedOptions (Options, inPackage, lang, optMake, dLanguage, antlrOpts, outDir), AntlrTarget (TS))
import BNFC.Utils (mkName, NameStyle (CamelCase), replace, (+.+), (+++))
import BNFC.Backend.Common.Makefile as MakeFile
import BNFC.Backend.Antlr (makeAntlr)
import BNFC.Backend.TypeScript.CFtoAbstract (cfToAbstract)
import BNFC.Backend.TypeScript.CFtoBuilder (cfToBuilder, mkBuildFnName)
import BNFC.Backend.TypeScript.CFtoPrinter (cfToPrinter)
import BNFC.Backend.Common.NamedVariables (firstUpperCase)

makeTypeScript :: SharedOptions -> CF -> MkFiles ()
makeTypeScript opts@Options{..} cf = do
    let packageBase = maybe id (+.+) inPackage pkgName
        dirBase = pkgToDir packageBase
 
    mkfile (dirBase </> "index.ts") makeTsComment indexTsContent
    makeAntlr (opts {dLanguage = TS, optMake = Nothing}) cf
    MakeFile.mkMakefile optMake makefileContent

    -- npm does not allow to have comments in package.json file (and cannot perform deps installation)
    -- so we have to create this file manually
    let finalDir = outDir </> dirBase
    liftIO $ createDirectoryIfMissing True finalDir
    liftIO $ writeFile (finalDir </> "package.json") (render packageJsonContent)

    mkfile (dirBase </> "abstract.ts") makeTsComment abstractContent
    mkfile (dirBase </> "builder.ts") makeTsComment builderContent
    mkfile (dirBase </> "printer.ts") makeTsComment printerContent

  where
    abstractContent = cfToAbstract cf
    datas = getAbstractSyntax cf
    builderContent = cfToBuilder cf opts
    printerContent = cfToPrinter cf
    pkgName = mkName [] CamelCase lang
    pkgToDir = replace '.' pathSeparator

    packageJsonContent = vcat
      [ "{"
      , nest 2 $ vcat
        [ text $ "\"name\": \"" ++ toLowerCase lang ++ "\","
        , "\"version\": \"1.0.0\","
        , "\"description\": \"\","
        , "\"main\": \"index.ts\","
        , "\"scripts\": {"
        , nest 2 "\"run\": \"ts-node index.ts\","
        , nest 2 "\"init\": \"tsc --init\""
        , "},"
        , "\"keywords\": [],"
        , "\"author\": \"\","
        , "\"license\": \"ISC\","
        , "\"dependencies\": {"
        , nest 2 "\"antlr4\": \"^4.13.1\""
        , "},"
        , "\"devDependencies\": {"
        , nest 2 "\"ts-node\": \"^10.9.1\","
        , nest 2 "\"typescript\": \"^5.2.2\""
        , "}"
        ]
      , "}"
      ]

    lexerClassName = lang ++ "GrammarLexer"
    parserClassName = lang ++ "GrammarParser"

    indexTsContent = vcat
      [ "import {CharStream, CommonTokenStream, ErrorListener, FileStream, RecognitionException, Recognizer} from 'antlr4'"
      , text $ "import " ++ lexerClassName ++ " from './" ++ lang ++ "Lexer'"
      , text $ "import " ++ parserClassName ++ " from './" ++ lang ++ "Parser'"
      , text $ "import {" ++ rootBuildFnName ++ "} from './builder'"
      , "import fs from 'fs'"
      , "import path from 'path'"
      , "import * as readline from 'readline/promises'"
      , ""
      , text $ "class" +++ errorListenerClassName ++ "<T> extends ErrorListener<T> {"
      , nest 2 $ vcat
        [ "constructor() {"
        , nest 2 "super()"
        , "}"
        , ""
        , "syntaxError(_recognizer: Recognizer<T>, _offendingSymbol: T, _line: number, _column: number, _msg: string, e: RecognitionException | undefined) {"
        , nest 2 "process.exit()"
        , "}"
        ]
      , "}"
      , ""
      , "async function getInput() {"
      , nest 2 $ vcat
        [ "const filename = process.argv[2]"
        , "if (filename) {"
        , nest 2 $ vcat
          [ "const fullPath = path.resolve(filename)"
          , "if (!fs.existsSync(fullPath)) {"
          , nest 2 "console.log(`file ${fullPath} does not exist`)"
          , nest 2 "process.exit()"
          , "}"
          , "return new FileStream(fullPath)"
          ]
        , "} else {"
        , nest 2 $ vcat
            [ "const rl = readline.createInterface(process.stdin, process.stdout)"
            , "const inputPromise = new Promise<string>(resolve => {"
            , nest 2 $ vcat
              [ "let input = ''"
              , "rl.on('line', (data) => {"
              , nest 2 "input += data"
              , "})"
              , ".on('close', () => {"
              , nest 2 "resolve(input)"
              , "})"
              ]
            , "})"
            , "const input = await inputPromise"
            , "return new CharStream(input)"
            ]
        , "}"
        ]
      , "}"
      , ""
      , "function createParser(charStream: CharStream) {"
      , nest 2 $ vcat
        [ "const lexer = new StellaGrammarLexer(charStream)"
        , text $ "lexer.addErrorListener(new" +++ errorListenerClassName ++ "())"
        , "const tokenStream = new CommonTokenStream(lexer)"
        , "const parser = new StellaGrammarParser(tokenStream)"
        , text $ "parser.addErrorListener(new" +++ errorListenerClassName ++ "())"
        , "return parser"
        ]
      , "}"
      , ""
      , "async function main() {"
      , nest 2 $ vcat
        [ "const input = await getInput()"
        , "const parser = createParser(input)"
        , "const ast = buildProgram(parser.program())"
        , "console.dir(ast, {depth: 6})"
        ]
      , "}"
      , ""
      , "main()"
      , ""
      ]
      where
        errorListenerClassName = firstUpperCase lang ++ "ErrorListener"
    rootBuildFnName = mkBuildFnName rootCat
    rootCat = fst (head datas)

    makeVars x = [MakeFile.mkVar n v | (n,v) <- x]
    makeRules x = [MakeFile.mkRule tar dep recipe  | (tar, dep, recipe) <- x]

    makefileVars = vcat $ makeVars
      [("LANG", lang)
      , ("LEXER_NAME", lang ++ "Lexer")
      , ("PARSER_NAME", lang ++ "Parser")
      , ("ANTLR4", "java org.antlr.v4.Tool")
      ]

    refVarWithPrefix :: String -> String
    refVarWithPrefix refVar = MakeFile.refVar "LANG" </> MakeFile.refVar refVar

    rmFile :: String -> String -> String
    rmFile refVar ext = "rm -f" +++ refVarWithPrefix refVar ++ ext

    makefileRules =  vcat $ makeRules
      [ (".PHONY", ["all", "clean", "remove"], [])
      , ("all", [MakeFile.refVar "LANG"], [])
      , ("lexer", [refVarWithPrefix "LEXER_NAME" ++ ".g4"], [MakeFile.refVar "ANTLR4" +++ "-Dlanguage=TypeScript" +++ refVarWithPrefix "LEXER_NAME" ++ ".g4"])
      , ("parser", [refVarWithPrefix "PARSER_NAME" ++ ".g4"], [MakeFile.refVar "ANTLR4" +++ "-Dlanguage=TypeScript" +++ refVarWithPrefix "PARSER_NAME" ++ ".g4"])
      , ("install-deps", [MakeFile.refVar "LANG" </> "package.json"], ["npm --prefix ./" ++ MakeFile.refVar "LANG" +++ "install" +++ MakeFile.refVar "LANG"])
      , ("init-ts-project", [MakeFile.refVar "LANG" </> "package.json"], ["cd" +++ MakeFile.refVar "LANG" +++ "&& npm run init" ])
      , (MakeFile.refVar "LANG", ["lexer", "parser", "install-deps", "init-ts-project"], [])
      , ("clean", [],
        [ "rm -rf" +++ MakeFile.refVar "LANG" </> "node_modules"
        , rmFile "LEXER_NAME" ".interp"
        , rmFile "LEXER_NAME" ".tokens"
        , rmFile "PARSER_NAME" ".interp"
        , rmFile "PARSER_NAME" ".tokens"
        , rmFile "LEXER_NAME" ".ts"
        , rmFile "PARSER_NAME" ".ts"
        , rmFile "PARSER_NAME" "Listener.ts"
        ])
      , ("remove", [], ["rm -rf" +++ MakeFile.refVar "LANG"])
      ]

    makefileContent _ = vcat [makefileVars, "", makefileRules, ""]

makeTsComment :: String -> String
makeTsComment = ("// TypeScript " ++)

toLowerCase :: String -> String
toLowerCase = map toLower
