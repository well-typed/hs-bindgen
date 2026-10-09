{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.A.Unsafe
    ( M.A.Unsafe.shape_draw
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.B
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <forward_decl/b.h>"
  , "void hs_bindgen_b1448c670abb4899 ("
  , "  struct shape *arg1"
  , ")"
  , "{"
  , "  (shape_draw)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_libraryforward_decl_M.A_Unsafe_shape_draw@
foreign import ccall unsafe "hs_bindgen_b1448c670abb4899" hs_bindgen_b1448c670abb4899_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_libraryforward_decl_M.A_Unsafe_shape_draw@
hs_bindgen_b1448c670abb4899 ::
     BG.Ptr M.B.Shape
  -> IO ()
hs_bindgen_b1448c670abb4899 =
  \x0 ->
    hs_bindgen_b1448c670abb4899_base (BG.toFFIType x0)

{-| __C declaration:__ @shape_draw@

    __defined at:__ @a.h 4:6@

    __exported by:__ @forward_decl\/b.h@
-}
shape_draw ::
     BG.Ptr M.B.Shape
     -- ^ __C declaration:__ @s@
  -> IO ()
shape_draw = hs_bindgen_b1448c670abb4899
