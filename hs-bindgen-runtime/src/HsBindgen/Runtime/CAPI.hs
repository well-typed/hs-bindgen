-- We capitalize module names, but use camelCase/PascalCase in code:
--
-- - in types names:    CapiFoo, FooCapiBar
-- - in variable names: capiFoo, fooCapiBar
module HsBindgen.Runtime.CAPI (
    addCSource
  , allocaAndPeek
    -- * Auxiliary
  , Data.List.unlines
) where

import Data.List qualified
import Foreign (Ptr, Storable, alloca, peek)
import Language.Haskell.TH (DecsQ)
import Language.Haskell.TH.Syntax (ForeignSrcLang (LangC), addForeignSource)

addCSource :: String -> DecsQ
addCSource src = do
    addForeignSource LangC src
    return []

allocaAndPeek :: Storable a => (Ptr a -> IO ()) -> IO a
allocaAndPeek kont = alloca $ \ptr -> kont ptr >> peek ptr
