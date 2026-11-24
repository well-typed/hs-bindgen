{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module IncludeCycle.A.FunPtr
    ( IncludeCycle.A.FunPtr.a_takes_b
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified IncludeCycle.B
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <include_cycle.h>"
  , "/* test_librarycyclesinclude_cycle_IncludeCycle.A_get_a_takes_b */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e5dc0422267fedee (void)) ("
  , "  struct b *arg1"
  , ")"
  , "{"
  , "  return &a_takes_b;"
  , "}"
  ]))

-- __unique:__ @test_librarycyclesinclude_cycle_IncludeCycle.A_get_a_takes_b@
foreign import ccall unsafe "hs_bindgen_e5dc0422267fedee" hs_bindgen_e5dc0422267fedee_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarycyclesinclude_cycle_IncludeCycle.A_get_a_takes_b@
hs_bindgen_e5dc0422267fedee :: IO (BG.FunPtr (BG.Ptr IncludeCycle.B.B -> IO ()))
hs_bindgen_e5dc0422267fedee =
  fmap BG.fromFFIType hs_bindgen_e5dc0422267fedee_base

{-# NOINLINE a_takes_b #-}
{-| __C declaration:__ @a_takes_b@

    __defined at:__ @a.h 18:6@

    __exported by:__ @include_cycle.h@
-}
a_takes_b :: BG.FunPtr (BG.Ptr IncludeCycle.B.B -> IO ())
a_takes_b =
  BG.unsafePerformIO hs_bindgen_e5dc0422267fedee
