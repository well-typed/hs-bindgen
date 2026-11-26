{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module IncludeCycle.A.Safe
    ( IncludeCycle.A.Safe.a_takes_b
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified IncludeCycle.B
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <include_cycle.h>"
  , "void hs_bindgen_405d5fd401f145e5 ("
  , "  struct b *arg1"
  , ")"
  , "{"
  , "  (a_takes_b)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_librarycyclesinclude_cycle_IncludeCycle.A_Safe_a_takes_b@
foreign import ccall safe "hs_bindgen_405d5fd401f145e5" hs_bindgen_405d5fd401f145e5_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_librarycyclesinclude_cycle_IncludeCycle.A_Safe_a_takes_b@
hs_bindgen_405d5fd401f145e5 ::
     BG.Ptr IncludeCycle.B.B
  -> IO ()
hs_bindgen_405d5fd401f145e5 =
  \x0 ->
    hs_bindgen_405d5fd401f145e5_base (BG.toFFIType x0)

{-| __C declaration:__ @a_takes_b@

    __defined at:__ @a.h 18:6@

    __exported by:__ @include_cycle.h@
-}
a_takes_b ::
     BG.Ptr IncludeCycle.B.B
     -- ^ __C declaration:__ @b@
  -> IO ()
a_takes_b = hs_bindgen_405d5fd401f145e5
