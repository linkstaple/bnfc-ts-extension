module BNFC.Backend.TypeScript.CFtoPrinter (cfToPrinter) where
import BNFC.CF (CF)
import Text.PrettyPrint.HughesPJClass (Doc, text)

cfToPrinter :: CF -> Doc
cfToPrinter _ = text ""
