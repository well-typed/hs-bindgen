{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HsBindgen.Runtime.Internal.Tuple (
    -- Zero-tuples
    Unit

    -- One-tuples
  , Solo(MkSolo)
  ) where

import Data.Tuple.Solo (Solo(MkSolo))

-- GHC 9.8 ships with @ghc-prim@ version 0.11 which provides the 'Unit' type. We
-- have to check the GHC version, because the `MIN_VERSION_ghc_prim` is not
-- defined when `ghc-prim` is not used. (And we must deactivate the package when
-- we don't use it to avoid unused package warnings).

#if __GLASGOW_HASKELL__ >=908
import GHC.Tuple (Unit)
#else
type Unit = ()
#endif
