module BNFC.Backend.TypeScript ( makeTypeScript ) where

import Control.Monad.Trans.Writer (tell, WriterT)

import BNFC.Backend.Base
import BNFC.CF
import BNFC.Options (SharedOptions)

makeTypeScript :: SharedOptions -> CF -> MkFiles ()
makeTypeScript _ _ = writeTsIndexFile

writeTsIndexFile :: WriterT [GeneratedFile] IO()
writeTsIndexFile = do
    tell [tsIndexFile]
    return ()
  where
    tsIndexFile = GeneratedFile
      { fileName    = "index.ts"
      , fileContent = "console.log('Hello world')"
      , makeComment = makeCommentLine
      }

makeCommentLine :: String -> String
makeCommentLine = ("// " ++)
