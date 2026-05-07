{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.f
    , Example.Unsafe.g
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/primitives/bool_macro_override.h>"
  , "void hs_bindgen_c170f90cc5412c59 ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (f)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_18fbcf94c0b76854 ("
  , "  A arg1,"
  , "  bool arg2"
  , ")"
  , "{"
  , "  (g)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_typesprimitivesbool_macro_ov_Example_Unsafe_f@
foreign import ccall unsafe "hs_bindgen_c170f90cc5412c59" hs_bindgen_c170f90cc5412c59_base ::
     RIP.Int32
  -> RIP.Word8
  -> IO ()

-- __unique:__ @test_typesprimitivesbool_macro_ov_Example_Unsafe_f@
hs_bindgen_c170f90cc5412c59 ::
     A
  -> RIP.CBool
  -> IO ()
hs_bindgen_c170f90cc5412c59 =
  RIP.fromFFIType hs_bindgen_c170f90cc5412c59_base

{-| __C declaration:__ @f@

    __defined at:__ @types\/primitives\/bool_macro_override.h 10:6@

    __exported by:__ @types\/primitives\/bool_macro_override.h@
-}
f ::
     A
     -- ^ __C declaration:__ @x@
  -> RIP.CBool
     -- ^ __C declaration:__ @y@
  -> IO ()
f = hs_bindgen_c170f90cc5412c59

-- __unique:__ @test_typesprimitivesbool_macro_ov_Example_Unsafe_g@
foreign import ccall unsafe "hs_bindgen_18fbcf94c0b76854" hs_bindgen_18fbcf94c0b76854_base ::
     RIP.Int32
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_typesprimitivesbool_macro_ov_Example_Unsafe_g@
hs_bindgen_18fbcf94c0b76854 ::
     A
  -> Bool'
  -> IO ()
hs_bindgen_18fbcf94c0b76854 =
  RIP.fromFFIType hs_bindgen_18fbcf94c0b76854_base

{-| __C declaration:__ @g@

    __defined at:__ @types\/primitives\/bool_macro_override.h 13:6@

    __exported by:__ @types\/primitives\/bool_macro_override.h@
-}
g ::
     A
     -- ^ __C declaration:__ @x@
  -> Bool'
     -- ^ __C declaration:__ @y@
  -> IO ()
g = hs_bindgen_18fbcf94c0b76854
