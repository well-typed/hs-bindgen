{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <ordinary_anon_parent.h>"
  , "/* get__acos_ptr */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_test_ordinary_anon_parent_c6a8256d628dc56b (void)) ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return &_acos;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_test_ordinary_anon_parent_c6a8256d628dc56b" hs_bindgen_test_ordinary_anon_parent_c6a8256d628dc56b ::
     IO (Ptr.FunPtr (FC.CDouble -> IO FC.CDouble))

{-# NOINLINE _acos_ptr #-}

{-| __C declaration:__ @_acos@

    __defined at:__ @ordinary_anon_child.h:4:1@

    __exported by:__ @ordinary_anon_parent.h@
-}
_acos_ptr :: Ptr.FunPtr (FC.CDouble -> IO FC.CDouble)
_acos_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_ordinary_anon_parent_c6a8256d628dc56b
