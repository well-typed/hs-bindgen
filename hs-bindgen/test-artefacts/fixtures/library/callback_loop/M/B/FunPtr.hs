{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.B.FunPtr
    ( M.B.FunPtr.b_visit
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.A
import M.B
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <root.h>"
  , "/* test_callback_loop_M.B_get_b_visit */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_390920e4176f5b5e (void)) ("
  , "  struct b *arg1,"
  , "  void (*arg2) ("
  , "  struct b *arg1,"
  , "  struct a *arg2"
  , ")"
  , ")"
  , "{"
  , "  return &b_visit;"
  , "}"
  ]))

-- __unique:__ @test_callback_loop_M.B_get_b_visit@
foreign import ccall unsafe "hs_bindgen_390920e4176f5b5e" hs_bindgen_390920e4176f5b5e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_callback_loop_M.B_get_b_visit@
hs_bindgen_390920e4176f5b5e :: IO (BG.FunPtr (BG.Ptr B -> BG.FunPtr (BG.Ptr B -> BG.Ptr M.A.A -> IO ()) -> IO ()))
hs_bindgen_390920e4176f5b5e =
  fmap BG.fromFFIType hs_bindgen_390920e4176f5b5e_base

{-# NOINLINE b_visit #-}
{-| __C declaration:__ @b_visit@

    __defined at:__ @b.h 6:6@

    __exported by:__ @root.h@
-}
b_visit :: BG.FunPtr (BG.Ptr B -> BG.FunPtr (BG.Ptr B -> BG.Ptr M.A.A -> IO ()) -> IO ())
b_visit =
  BG.unsafePerformIO hs_bindgen_390920e4176f5b5e
