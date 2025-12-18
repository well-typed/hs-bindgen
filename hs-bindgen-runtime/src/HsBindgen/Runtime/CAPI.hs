module HsBindgen.Runtime.CAPI (
    addCSource,
    allocaAndPeek,
    allocaAndPeekConst,
) where

import Foreign (Ptr, Storable, alloca, peek)
import Language.Haskell.TH (DecsQ)
import Language.Haskell.TH.Syntax (ForeignSrcLang (LangC), addForeignSource)

import HsBindgen.Runtime.ConstPtr (ConstPtr (ConstPtr))

addCSource :: String -> DecsQ
addCSource src = do
    addForeignSource LangC src
    return []

allocaAndPeek :: Storable a => (Ptr a -> IO ()) -> IO a
allocaAndPeek kont = alloca $ \ptr -> kont ptr >> peek ptr

allocaAndPeekConst :: Storable a => (ConstPtr a -> IO ()) -> IO a
allocaAndPeekConst kont = alloca $ \ptr -> kont (ConstPtr ptr) >> peek ptr
