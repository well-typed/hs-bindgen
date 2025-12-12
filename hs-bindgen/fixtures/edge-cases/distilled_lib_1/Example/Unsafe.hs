{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import GHC.Int (Int32)
import GHC.Word (Word32)
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/distilled_lib_1.h>"
  , "int32_t hs_bindgen_2a91c367a9380a63 ("
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
foreign import ccall unsafe "hs_bindgen_2a91c367a9380a63" some_fun_base ::
     Ptr.Ptr Void
  -> Word32
  -> Ptr.Ptr Void
  -> IO Int32

{-| __C declaration:__ @some_fun@

    __defined at:__ @edge-cases\/distilled_lib_1.h:72:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@

    __unique:__ @test_edgecasesdistilled_lib_1_Example_Unsafe_some_fun@
-}
some_fun ::
     Ptr.Ptr A_type_t
     -- ^ __C declaration:__ @i@
  -> HsBindgen.Runtime.Prelude.Word32
     -- ^ __C declaration:__ @j@
  -> Ptr.Ptr HsBindgen.Runtime.Prelude.Word8
     -- ^ __C declaration:__ @k@
  -> IO HsBindgen.Runtime.Prelude.Int32
some_fun =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType some_fun_base
