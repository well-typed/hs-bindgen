module HsBindgen.Runtime.CAPI (
    addCSource,
) where

import Language.Haskell.TH (DecsQ)
import Language.Haskell.TH.Syntax (addForeignSource, ForeignSrcLang(LangC))

addCSource :: String -> DecsQ
addCSource src = do
    addForeignSource LangC src
    return []
