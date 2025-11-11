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
  [ "#include <definitions.h>"
  , "/* get_foo_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_definitions_32925a42980e81cd (void)) ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_test_definitions_32925a42980e81cd" hs_bindgen_test_definitions_32925a42980e81cd ::
     IO (Ptr.FunPtr (FC.CDouble -> IO FC.CInt))

{-# NOINLINE foo_ptr #-}

{-| __C declaration:__ @foo@

    __defined at:__ @definitions.h:13:5@

    __exported by:__ @definitions.h@
-}
foo_ptr :: Ptr.FunPtr (FC.CDouble -> IO FC.CInt)
foo_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_definitions_32925a42980e81cd
