{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.B.Unsafe
    ( M.B.Unsafe.b_visit
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.A
import M.B
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <root.h>"
  , "void hs_bindgen_d568dc14adb38f63 ("
  , "  struct b *arg1,"
  , "  void (*arg2) ("
  , "  struct b *arg1,"
  , "  struct a *arg2"
  , ")"
  , ")"
  , "{"
  , "  (b_visit)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_callback_loop_M.B_Unsafe_b_visit@
foreign import ccall unsafe "hs_bindgen_d568dc14adb38f63" hs_bindgen_d568dc14adb38f63_base ::
     BG.Ptr BG.Void
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_callback_loop_M.B_Unsafe_b_visit@
hs_bindgen_d568dc14adb38f63 ::
     BG.Ptr B
  -> BG.FunPtr (BG.Ptr B -> BG.Ptr M.A.A -> IO ())
  -> IO ()
hs_bindgen_d568dc14adb38f63 =
  \x0 ->
    \x1 ->
      hs_bindgen_d568dc14adb38f63_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @b_visit@

    __defined at:__ @b.h 6:6@

    __exported by:__ @root.h@
-}
b_visit ::
     BG.Ptr B
     -- ^ __C declaration:__ @b@
  -> BG.FunPtr (BG.Ptr B -> BG.Ptr M.A.A -> IO ())
     -- ^ __C declaration:__ @cb@
  -> IO ()
b_visit = hs_bindgen_d568dc14adb38f63
