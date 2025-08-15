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
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.SizedByteArray
import Prelude ((<*>), Eq, IO, Int, Show, pure)

$(CAPI.addCSource "#include <definitions.h>\nsigned int hs_bindgen_test_definitions_a7d624773bb0585c (double arg1) { return foo(arg1); }\n__attribute__ ((const)) signed int *get_n_ptr (void) { return &n; } \n")

foreign import ccall safe "hs_bindgen_test_definitions_a7d624773bb0585c" foo :: FC.CDouble -> IO FC.CInt

foreign import ccall safe "get_n_ptr" n_ptr :: F.Ptr FC.CInt

data X = X
  { x_n :: FC.CInt
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

newtype Y = Y
  { un_Y :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable Y

{-|

  __See:__ 'set_y_m'

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

-}
get_y_o :: Y -> FC.CInt
get_y_o = HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_y_o'

-}
set_y_o :: FC.CInt -> Y
set_y_o = HsBindgen.Runtime.ByteArray.setUnionPayload
