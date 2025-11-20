{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/distilled_lib_1.h>"
  , "int32_t hs_bindgen_test_edgecasesdistilled_lib_1_29c178c31334688f ("
  , "  a_type_t *arg1,"
  , "  uint32_t arg2,"
  , "  uint8_t *arg3"
  , ")"
  , "{"
  , "  return some_fun(arg1, arg2, arg3);"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesdistilled_lib_1_29c178c31334688f" some_fun_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr A_type_t
    -> HsBindgen.Runtime.Prelude.Word32
    -> Ptr.Ptr HsBindgen.Runtime.Prelude.Word8
    -> IO HsBindgen.Runtime.Prelude.Int32
    )

{-| __C declaration:__ @some_fun@

    __defined at:__ @edge-cases\/distilled_lib_1.h:72:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
some_fun ::
     Ptr.Ptr A_type_t
     {- ^ __C declaration:__ @i@
     -}
  -> HsBindgen.Runtime.Prelude.Word32
     {- ^ __C declaration:__ @j@
     -}
  -> Ptr.Ptr HsBindgen.Runtime.Prelude.Word8
     {- ^ __C declaration:__ @k@
     -}
  -> IO HsBindgen.Runtime.Prelude.Int32
some_fun =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType some_fun_base
