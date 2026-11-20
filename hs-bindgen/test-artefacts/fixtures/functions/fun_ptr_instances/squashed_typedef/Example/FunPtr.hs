{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.h
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/fun_ptr_instances/squashed_typedef.h>"
  , "/* test_functionsfun_ptr_instancessq_Example_get_h */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d4648cb235c86cb6 (void)) ("
  , "  void (*arg1) ("
  , "  T *arg1"
  , ")"
  , ")"
  , "{"
  , "  return &h;"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_ptr_instancessq_Example_get_h@
foreign import ccall unsafe "hs_bindgen_d4648cb235c86cb6" hs_bindgen_d4648cb235c86cb6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_ptr_instancessq_Example_get_h@
hs_bindgen_d4648cb235c86cb6 :: IO (BG.FunPtr (BG.FunPtr (BG.Ptr T -> IO ()) -> IO ()))
hs_bindgen_d4648cb235c86cb6 =
  fmap BG.fromFFIType hs_bindgen_d4648cb235c86cb6_base

{-# NOINLINE h #-}
{-| __C declaration:__ @h@

    __defined at:__ @functions\/fun_ptr_instances\/squashed_typedef.h 9:6@

    __exported by:__ @functions\/fun_ptr_instances\/squashed_typedef.h@
-}
h :: BG.FunPtr (BG.FunPtr (BG.Ptr T -> IO ()) -> IO ())
h = BG.unsafePerformIO hs_bindgen_d4648cb235c86cb6
