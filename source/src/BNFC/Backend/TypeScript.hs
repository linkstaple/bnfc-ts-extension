{-# LANGUAGE RecordWildCards #-}

module BNFC.Backend.TypeScript ( makeTypeScript ) where

import Prelude hiding ((<>))
import Text.PrettyPrint ( text, vcat, Doc )
import System.FilePath ((</>), pathSeparator)
import Data.Char (toLower)

import BNFC.Backend.Base
import BNFC.CF ( CF )
import BNFC.Options (SharedOptions (Options, inPackage, lang, optMake))
import BNFC.Utils (mkName, NameStyle (CamelCase), replace, (+.+), (+++))
import BNFC.Backend.Antlr (makeAntlr)
import BNFC.Backend.Common.Makefile as MakeFile

makeTypeScript :: SharedOptions -> CF -> MkFiles ()
makeTypeScript opts@Options{..} cf = do
    makeAntlr opts cf
    let packageBase = maybe id (+.+) inPackage pkgName
        dirBase = pkgToDir packageBase
 
    mkfile (dirBase </> "package.json") makeJsonComment packageJsonContent
    mkfile (dirBase </> "index.ts") makeTsComment indexTsContent
    MakeFile.mkMakefile optMake makefileContent

  where
    pkgName = mkName [] CamelCase lang
    pkgToDir = replace '.' pathSeparator

    packageJsonContent = vcat
      [ "{"
      , indent 2 $ "\"name\": \"" ++ toLowerCase lang ++ "\","
      , indent 2 "\"version\": \"1.0.0\","
      , indent 2 "\"description\": \"\","
      , indent 2 "\"main\": \"index.ts\","
      , indent 2 "\"scripts\": {"
      , indent 4 "\"run\": \"ts-node index.ts\""
      , indent 2 "},"
      , indent 2 "\"keywords\": [],"
      , indent 2 "\"author\": \"\","
      , indent 2 "\"license\": \"ISC\","
      , indent 2 "\"dependencies\": {"
      , indent 4 "\"antlr4\": \"^4.13.1\""
      , indent 2 "},"
      , indent 2 "\"devDependencies\": {"
      , indent 4 "\"ts-node\": \"^10.9.1\","
      , indent 4 "\"typescript\": \"^5.2.2\""
      , indent 2 "}"
      , "}"
      ]

    lexerClassName = lang ++ "GrammarLexer"
    parserClassName = lang ++ "GrammarParser"

    indexTsContent = vcat
      [ "import { CharStream, CommonTokenStream } from 'antlr4'"
      , text $ "import " ++ lexerClassName ++ " from './" ++ lang ++ "Lexer'"
      , text $ "import " ++ parserClassName ++ " from './" ++ lang ++ "Parser'"
      , ""
      , "const input = 'your text here'"
      , "const chars = new CharStream(input) // replace this with a FileStream as required"
      , text $ "const lexer = new " ++ lexerClassName ++ "(chars)"
      , "const tokens = new CommonTokenStream(lexer)"
      , text $ "const parser = new " ++ parserClassName ++ "(tokens)"
      , "const tree = parser.startRule()"
      , ""
      ]

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
      , (MakeFile.refVar "LANG", ["lexer", "parser", "install-deps"], [])
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
makeTsComment = ("// -- TypeScript -- " ++)

makeJsonComment :: String -> String
makeJsonComment = ("// -- JSON -- " ++)

-- add N spaces before line
indent :: Int -> String -> Doc
indent size = text . (replicate size ' ' ++)

toLowerCase :: String -> String
toLowerCase = map toLower
