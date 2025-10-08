{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example where

import qualified Data.Array.Byte
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.Prelude
import qualified HsBindgen.Runtime.SizedByteArray
import Prelude ((<*>), Eq, IO, Int, Show, pure)

$(HsBindgen.Runtime.Prelude.addCSource "#include <definitions.h>\nsigned int hs_bindgen_test_definitions_5a514c66396155ff (double arg1) { return foo(arg1); }\n/* get_foo_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_definitions_32925a42980e81cd (void)) (double arg1) { return &foo; } \n/* get_n_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_definitions_cfec0f95f22bb37c (void) { return &n; } \n")

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
get_y_m
  :: Y
  -> FC.CInt
get_y_m = HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_y_m'

-}
set_y_m
  :: FC.CInt
  -> Y
set_y_m = HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  __See:__ 'set_y_o'

__C declaration:__ @o@

__defined at:__ @definitions.h:26:22@

__exported by:__ @definitions.h@
-}
get_y_o
  :: Y
  -> FC.CInt
get_y_o = HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_y_o'

-}
set_y_o
  :: FC.CInt
  -> Y
set_y_o = HsBindgen.Runtime.ByteArray.setUnionPayload

{-| __C declaration:__ @foo@

    __defined at:__ @definitions.h:13:5@

    __exported by:__ @definitions.h@
-}
foreign import ccall safe "hs_bindgen_test_definitions_5a514c66396155ff" foo
  :: FC.CDouble
     {- ^ __C declaration:__ @x@
     -}
  -> IO FC.CInt

foreign import ccall unsafe "hs_bindgen_test_definitions_32925a42980e81cd" hs_bindgen_test_definitions_32925a42980e81cd
  :: IO (Ptr.FunPtr (FC.CDouble -> IO FC.CInt))

{-# NOINLINE foo_ptr #-}

{-| __C declaration:__ @foo@

    __defined at:__ @definitions.h:13:5@

    __exported by:__ @definitions.h@
-}
foo_ptr :: Ptr.FunPtr (FC.CDouble -> IO FC.CInt)
foo_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_definitions_32925a42980e81cd

foreign import ccall unsafe "hs_bindgen_test_definitions_cfec0f95f22bb37c" hs_bindgen_test_definitions_cfec0f95f22bb37c
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE n_ptr #-}

{-| __C declaration:__ @n@

    __defined at:__ @definitions.h:18:5@

    __exported by:__ @definitions.h@
-}
n_ptr :: Ptr.Ptr FC.CInt
n_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_definitions_cfec0f95f22bb37c
