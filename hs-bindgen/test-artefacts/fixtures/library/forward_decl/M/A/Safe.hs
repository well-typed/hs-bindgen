{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.A.Safe
    ( M.A.Safe.shape_draw
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.B
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <forward_decl/b.h>"
  , "void hs_bindgen_ace074fd2b4c2655 ("
  , "  struct shape *arg1"
  , ")"
  , "{"
  , "  (shape_draw)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_libraryforward_decl_M.A_Safe_shape_draw@
foreign import ccall safe "hs_bindgen_ace074fd2b4c2655" hs_bindgen_ace074fd2b4c2655_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_libraryforward_decl_M.A_Safe_shape_draw@
hs_bindgen_ace074fd2b4c2655 ::
     BG.Ptr M.B.Shape
  -> IO ()
hs_bindgen_ace074fd2b4c2655 =
  \x0 ->
    hs_bindgen_ace074fd2b4c2655_base (BG.toFFIType x0)

{-| __C declaration:__ @shape_draw@

    __defined at:__ @a.h 4:6@

    __exported by:__ @forward_decl\/b.h@
-}
shape_draw ::
     BG.Ptr M.B.Shape
     -- ^ __C declaration:__ @s@
  -> IO ()
shape_draw = hs_bindgen_ace074fd2b4c2655
