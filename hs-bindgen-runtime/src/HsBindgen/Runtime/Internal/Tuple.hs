{-# LANGUAGE CPP #-}

module HsBindgen.Runtime.Internal.Tuple (
    Unit
  , Solo(MkSolo)
  ) where

-- The package @ghc-prim@ is problematic.
--
-- Also interesting, not all package versions are available on Hackage:
-- https://hackage.haskell.org/package/ghc-prim. In particular, version @0.9.1@
-- is missing.

#define IS_GHC_PRIM_0_9_X  (MIN_VERSION_ghc_prim(0,9,0)  && !MIN_VERSION_ghc_prim(0,10,0))
#define IS_GHC_PRIM_0_10_X (MIN_VERSION_ghc_prim(0,10,0) && !MIN_VERSION_ghc_prim(0,11,0))

#if IS_GHC_PRIM_0_9_X
import GHC.Tuple (Solo (Solo))

type Unit = ()

pattern MkSolo :: a -> Solo a
pattern MkSolo x = Solo x
{-# COMPLETE MkSolo #-}
#endif

#if IS_GHC_PRIM_0_10_X
import GHC.Tuple (Solo (MkSolo))

type Unit = ()
#endif

#if MIN_VERSION_ghc_prim(0,11,0)
import GHC.Tuple (Solo (MkSolo), Unit)
#endif
