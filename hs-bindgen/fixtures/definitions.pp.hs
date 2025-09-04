{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Data.Array.Byte
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as F
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.SizedByteArray
import Prelude ((<*>), Eq, IO, Int, Show, pure)

$(CAPI.addCSource "#include <definitions.h>\nsigned int hs_bindgen_test_definitions_a7d624773bb0585c (double arg1) { return foo(arg1); }\n/* get_foo_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_definitions_fb3e409881d8c524 (void)) (double arg1) { return &foo; } \n/* get_n_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_definitions_fc2aad2af9befead (void) { return &n; } \n")

{-| __C declaration:__ @foo@

    __defined at:__ @definitions.h:13:5@

    __exported by:__ @definitions.h@
-}
foreign import ccall safe "hs_bindgen_test_definitions_a7d624773bb0585c" foo
  :: FC.CDouble
     {- ^ __C declaration:__ @x@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @foo@

    __defined at:__ @definitions.h:13:5@

    __exported by:__ @definitions.h@
-}
foreign import ccall unsafe "hs_bindgen_test_definitions_fb3e409881d8c524" hs_bindgen_test_definitions_fb3e409881d8c524
  :: IO (F.FunPtr (FC.CDouble -> IO FC.CInt))

{-# NOINLINE foo_ptr #-}

foo_ptr :: F.FunPtr (FC.CDouble -> IO FC.CInt)
foo_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_definitions_fb3e409881d8c524

{-| __C declaration:__ @n@

    __defined at:__ @definitions.h:18:5@

    __exported by:__ @definitions.h@
-}
foreign import ccall unsafe "hs_bindgen_test_definitions_fc2aad2af9befead" hs_bindgen_test_definitions_fc2aad2af9befead
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE n_ptr #-}

n_ptr :: F.Ptr FC.CInt
n_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_definitions_fc2aad2af9befead

{-| __C declaration:__ @X@

    __defined at:__ @definitions.h:23:8@

    __exported by:__ @definitions.h@
-}
data X = X
  { x_n :: FC.CInt
    {- ^ __C declaration:__ @n@

         __defined at:__ @definitions.h:23:16@

         __exported by:__ @definitions.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable X where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure X
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          X x_n2 -> F.pokeByteOff ptr0 (0 :: Int) x_n2

{-| __C declaration:__ @Y@

    __defined at:__ @definitions.h:26:7@

    __exported by:__ @definitions.h@
-}
newtype Y = Y
  { un_Y :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable Y

{-|

  __See:__ 'set_y_m'

__C declaration:__ @m@

__defined at:__ @definitions.h:26:15@

__exported by:__ @definitions.h@
-}
get_y_m :: Y -> FC.CInt
get_y_m = HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_y_m'

-}
set_y_m :: FC.CInt -> Y
set_y_m = HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  __See:__ 'set_y_o'

__C declaration:__ @o@

__defined at:__ @definitions.h:26:22@

__exported by:__ @definitions.h@
-}
get_y_o :: Y -> FC.CInt
get_y_o = HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_y_o'

-}
set_y_o :: FC.CInt -> Y
set_y_o = HsBindgen.Runtime.ByteArray.setUnionPayload
