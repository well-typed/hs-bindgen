{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Early.FunPtr
    ( M.Early.FunPtr.early_draw
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.Alias
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <root.h>"
  , "/* test_squash_outside_M.Early_get_early_draw */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5f200d345111c599 (void)) ("
  , "  struct window *arg1"
  , ")"
  , "{"
  , "  return &early_draw;"
  , "}"
  ]))

-- __unique:__ @test_squash_outside_M.Early_get_early_draw@
foreign import ccall unsafe "hs_bindgen_5f200d345111c599" hs_bindgen_5f200d345111c599_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_squash_outside_M.Early_get_early_draw@
hs_bindgen_5f200d345111c599 :: IO (BG.FunPtr (BG.Ptr M.Alias.Window -> IO ()))
hs_bindgen_5f200d345111c599 =
  fmap BG.fromFFIType hs_bindgen_5f200d345111c599_base

{-# NOINLINE early_draw #-}
{-| __C declaration:__ @early_draw@

    __defined at:__ @lib\/early.h 5:6@

    __exported by:__ @root.h@
-}
early_draw :: BG.FunPtr (BG.Ptr M.Alias.Window -> IO ())
early_draw =
  BG.unsafePerformIO hs_bindgen_5f200d345111c599
