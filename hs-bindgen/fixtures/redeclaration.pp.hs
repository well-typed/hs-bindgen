{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example where

import qualified Data.Array.Byte
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.Prelude
import qualified HsBindgen.Runtime.SizedByteArray
import Data.Bits (FiniteBits)
import Prelude ((<*>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure)

$(HsBindgen.Runtime.Prelude.addCSource "#include <redeclaration.h>\n/* get_x_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_redeclaration_10b125673bf2041b (void) { return &x; } \n")

{-| __C declaration:__ @int_t@

    __defined at:__ @redeclaration.h:20:13@

    __exported by:__ @redeclaration.h@
-}
newtype Int_t = Int_t
  { un_Int_t :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @X@

    __defined at:__ @redeclaration.h:26:8@

    __exported by:__ @redeclaration.h@
-}
data X = X
  { x_n :: FC.CInt
    {- ^ __C declaration:__ @n@

         __defined at:__ @redeclaration.h:26:16@

         __exported by:__ @redeclaration.h@
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

{-| __C declaration:__ @y@

    __defined at:__ @redeclaration.h:29:7@

    __exported by:__ @redeclaration.h@
-}
data Y

{-| __C declaration:__ @Y@

    __defined at:__ @redeclaration.h:30:7@

    __exported by:__ @redeclaration.h@
-}
newtype Y = Y
  { un_Y :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable Y

{-|

  __See:__ 'set_y_m'

__C declaration:__ @m@

__defined at:__ @redeclaration.h:30:15@

__exported by:__ @redeclaration.h@
-}
get_y_m ::
     Y
  -> FC.CInt
get_y_m = HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_y_m'

-}
set_y_m ::
     FC.CInt
  -> Y
set_y_m = HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  __See:__ 'set_y_o'

__C declaration:__ @o@

__defined at:__ @redeclaration.h:30:22@

__exported by:__ @redeclaration.h@
-}
get_y_o ::
     Y
  -> FC.CInt
get_y_o = HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_y_o'

-}
set_y_o ::
     FC.CInt
  -> Y
set_y_o = HsBindgen.Runtime.ByteArray.setUnionPayload

foreign import ccall unsafe "hs_bindgen_test_redeclaration_10b125673bf2041b" hs_bindgen_test_redeclaration_10b125673bf2041b ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE x_ptr #-}

{-| __C declaration:__ @x@

    __defined at:__ @redeclaration.h:11:5@

    __exported by:__ @redeclaration.h@
-}
x_ptr :: Ptr.Ptr FC.CInt
x_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_redeclaration_10b125673bf2041b
