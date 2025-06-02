module HsBindgen.Runtime.CAPI (
    addCSource,
    allocaAndPeek,
) where

import Language.Haskell.TH (DecsQ)
import Language.Haskell.TH.Syntax (addForeignSource, ForeignSrcLang(LangC))
import Foreign (Storable, Ptr, peek, alloca)

addCSource :: String -> DecsQ
addCSource src = do
    addForeignSource LangC src
    return []

allocaAndPeek :: Storable a => (Ptr a -> IO ()) -> IO a
allocaAndPeek kont = alloca $ \ptr -> kont ptr >> peek ptr
