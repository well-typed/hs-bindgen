{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.A.Unsafe
    ( M.A.Unsafe.a_visit
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.B
import M.A
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <root.h>"
  , "void hs_bindgen_95f65463dc04220d ("
  , "  struct a *arg1,"
  , "  void (*arg2) ("
  , "  struct a *arg1,"
  , "  struct b *arg2"
  , ")"
  , ")"
  , "{"
  , "  (a_visit)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_callback_loop_M.A_Unsafe_a_visit@
foreign import ccall unsafe "hs_bindgen_95f65463dc04220d" hs_bindgen_95f65463dc04220d_base ::
     BG.Ptr BG.Void
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_callback_loop_M.A_Unsafe_a_visit@
hs_bindgen_95f65463dc04220d ::
     BG.Ptr A
  -> BG.FunPtr (BG.Ptr A -> BG.Ptr M.B.B -> IO ())
  -> IO ()
hs_bindgen_95f65463dc04220d =
  \x0 ->
    \x1 ->
      hs_bindgen_95f65463dc04220d_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @a_visit@

    __defined at:__ @a.h 6:6@

    __exported by:__ @root.h@
-}
a_visit ::
     BG.Ptr A
     -- ^ __C declaration:__ @a@
  -> BG.FunPtr (BG.Ptr A -> BG.Ptr M.B.B -> IO ())
     -- ^ __C declaration:__ @cb@
  -> IO ()
a_visit = hs_bindgen_95f65463dc04220d
