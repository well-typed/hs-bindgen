{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.A.FunPtr
    ( M.A.FunPtr.a_visit
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.B
import M.A
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <root.h>"
  , "/* test_callback_loop_M.A_get_a_visit */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4fd583c08abd8df1 (void)) ("
  , "  struct a *arg1,"
  , "  void (*arg2) ("
  , "  struct a *arg1,"
  , "  struct b *arg2"
  , ")"
  , ")"
  , "{"
  , "  return &a_visit;"
  , "}"
  ]))

-- __unique:__ @test_callback_loop_M.A_get_a_visit@
foreign import ccall unsafe "hs_bindgen_4fd583c08abd8df1" hs_bindgen_4fd583c08abd8df1_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_callback_loop_M.A_get_a_visit@
hs_bindgen_4fd583c08abd8df1 :: IO (BG.FunPtr (BG.Ptr A -> BG.FunPtr (BG.Ptr A -> BG.Ptr M.B.B -> IO ()) -> IO ()))
hs_bindgen_4fd583c08abd8df1 =
  fmap BG.fromFFIType hs_bindgen_4fd583c08abd8df1_base

{-# NOINLINE a_visit #-}
{-| __C declaration:__ @a_visit@

    __defined at:__ @a.h 6:6@

    __exported by:__ @root.h@
-}
a_visit :: BG.FunPtr (BG.Ptr A -> BG.FunPtr (BG.Ptr A -> BG.Ptr M.B.B -> IO ()) -> IO ())
a_visit =
  BG.unsafePerformIO hs_bindgen_4fd583c08abd8df1
