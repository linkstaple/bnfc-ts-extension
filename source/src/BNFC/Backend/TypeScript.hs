{-# LANGUAGE RecordWildCards #-}

module BNFC.Backend.TypeScript ( makeTypeScript ) where

import Prelude hiding ((<>))
import Text.PrettyPrint ( text, vcat, Doc )
import System.FilePath ((</>), pathSeparator)
import Data.Char (toLower)

import BNFC.Backend.Base
import BNFC.CF
import BNFC.Options (SharedOptions (Options, inPackage, lang))
import BNFC.Utils (mkName, NameStyle (CamelCase), replace, (+.+))

makeTypeScript :: SharedOptions -> CF -> MkFiles ()
makeTypeScript Options{..} _ = do
    let packageBase = maybe id (+.+) inPackage pkgName
        dirBase = pkgToDir packageBase
 
    mkfile (dirBase </> "package.json") makeJsonComment packageJsonContent
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
      , indent 4 "\"antlr4\": \"^0.5.0-alpha.4\""
      , indent 2 "},"
      , indent 2 "\"devDependencies\": {"
      , indent 4 "\"ts-node\": \"^10.9.1\","
      , indent 4 "\"typescript\": \"^5.2.2\""
      , indent 2 "}"
      , "}"
      ]

makeTsComment :: String -> String
makeTsComment = ("// -- TypeScript -- " ++)

makeJsonComment :: String -> String
makeJsonComment = ("// -- JSON -- " ++)

-- add N spaces before line
indent :: Int -> String -> Doc
indent size = text . (replicate size ' ' ++)

toLowerCase :: String -> String
toLowerCase = map toLower
