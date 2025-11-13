{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module RPM.Sw where

import qualified Data.Array.Byte
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.Prelude
import qualified HsBindgen.Runtime.SizedByteArray
import Data.Bits (FiniteBits)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @__time_t@

    __defined at:__ @bits\/types.h:160:26@

    __exported by:__ @rpm\/rpmsw.h@
-}
newtype C__Time_t = C__Time_t
  { un_C__Time_t :: FC.CLong
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @__suseconds_t@

    __defined at:__ @bits\/types.h:162:31@

    __exported by:__ @rpm\/rpmsw.h@
-}
newtype C__Suseconds_t = C__Suseconds_t
  { un_C__Suseconds_t :: FC.CLong
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @__ssize_t@

    __defined at:__ @bits\/types.h:194:27@

    __exported by:__ @rpm\/rpmsw.h@
-}
newtype C__Ssize_t = C__Ssize_t
  { un_C__Ssize_t :: FC.CLong
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @timeval@

    __defined at:__ @bits\/types\/struct_timeval.h:8:8@

    __exported by:__ @rpm\/rpmsw.h@
-}
data Timeval = Timeval
  { timeval_tv_sec :: C__Time_t
    {- ^ __C declaration:__ @tv_sec@

         __defined at:__ @bits\/types\/struct_timeval.h:14:12@

         __exported by:__ @rpm\/rpmsw.h@
    -}
  , timeval_tv_usec :: C__Suseconds_t
    {- ^ __C declaration:__ @tv_usec@

         __defined at:__ @bits\/types\/struct_timeval.h:15:17@

         __exported by:__ @rpm\/rpmsw.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Timeval where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Timeval
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Timeval timeval_tv_sec2 timeval_tv_usec3 ->
               F.pokeByteOff ptr0 (0 :: Int) timeval_tv_sec2
            >> F.pokeByteOff ptr0 (8 :: Int) timeval_tv_usec3

{-| __C declaration:__ @ssize_t@

    __defined at:__ @unistd.h:220:19@

    __exported by:__ @rpm\/rpmsw.h@
-}
newtype Ssize_t = Ssize_t
  { un_Ssize_t :: C__Ssize_t
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-|

  > rpmsw

__C declaration:__ @rpmtime_t@

__defined at:__ @rpm\/rpmsw.h:19:27@

__exported by:__ @rpm\/rpmsw.h@
-}
newtype Rpmtime_t = Rpmtime_t
  { un_Rpmtime_t :: FC.CULong
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-|

  > rpmsw

__C declaration:__ @rpmsw@

__defined at:__ @rpm\/rpmsw.h:23:26@

__exported by:__ @rpm\/rpmsw.h@
-}
newtype Rpmsw = Rpmsw
  { un_Rpmsw :: Ptr.Ptr Rpmsw_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-|

  > rpmsw

__C declaration:__ @rpmop@

__defined at:__ @rpm\/rpmsw.h:27:26@

__exported by:__ @rpm\/rpmsw.h@
-}
newtype Rpmop = Rpmop
  { un_Rpmop :: Ptr.Ptr Rpmop_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __defined at:__ @rpm\/rpmsw.h:32:5@

    __exported by:__ @rpm\/rpmsw.h@
-}
newtype Rpmsw_s_u = Rpmsw_s_u
  { un_Rpmsw_s_u :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 16) 8 instance F.Storable Rpmsw_s_u

{-|

  __See:__ 'set_rpmsw_s_u_tv'

__C declaration:__ @tv@

__defined at:__ @rpm\/rpmsw.h:33:17@

__exported by:__ @rpm\/rpmsw.h@
-}
get_rpmsw_s_u_tv ::
     Rpmsw_s_u
  -> Timeval
get_rpmsw_s_u_tv =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_rpmsw_s_u_tv'

-}
set_rpmsw_s_u_tv ::
     Timeval
  -> Rpmsw_s_u
set_rpmsw_s_u_tv =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  __See:__ 'set_rpmsw_s_u_ticks'

__C declaration:__ @ticks@

__defined at:__ @rpm\/rpmsw.h:34:25@

__exported by:__ @rpm\/rpmsw.h@
-}
get_rpmsw_s_u_ticks ::
     Rpmsw_s_u
  -> FC.CULLong
get_rpmsw_s_u_ticks =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_rpmsw_s_u_ticks'

-}
set_rpmsw_s_u_ticks ::
     FC.CULLong
  -> Rpmsw_s_u
set_rpmsw_s_u_ticks =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  __See:__ 'set_rpmsw_s_u_tocks'

__C declaration:__ @tocks@

__defined at:__ @rpm\/rpmsw.h:35:20@

__exported by:__ @rpm\/rpmsw.h@
-}
get_rpmsw_s_u_tocks ::
     Rpmsw_s_u
  -> (HsBindgen.Runtime.ConstantArray.ConstantArray 2) FC.CULong
get_rpmsw_s_u_tocks =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_rpmsw_s_u_tocks'

-}
set_rpmsw_s_u_tocks ::
     (HsBindgen.Runtime.ConstantArray.ConstantArray 2) FC.CULong
  -> Rpmsw_s_u
set_rpmsw_s_u_tocks =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  > rpmsw

__C declaration:__ @rpmsw_s@

__defined at:__ @rpm\/rpmsw.h:31:8@

__exported by:__ @rpm\/rpmsw.h@
-}
data Rpmsw_s = Rpmsw_s
  { rpmsw_s_u :: Rpmsw_s_u
    {- ^ Private

    __C declaration:__ @u@

    __defined at:__ @rpm\/rpmsw.h:36:7@

    __exported by:__ @rpm\/rpmsw.h@
    -}
  }

instance F.Storable Rpmsw_s where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Rpmsw_s
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Rpmsw_s rpmsw_s_u2 ->
            F.pokeByteOff ptr0 (0 :: Int) rpmsw_s_u2

{-|

  > rpmsw

  Cumulative statistics for an operation.

__C declaration:__ @rpmop_s@

__defined at:__ @rpm\/rpmsw.h:42:8@

__exported by:__ @rpm\/rpmsw.h@
-}
data Rpmop_s = Rpmop_s
  { rpmop_s_begin :: Rpmsw_s
    {- ^ Starting time stamp.

    __C declaration:__ @begin@

    __defined at:__ @rpm\/rpmsw.h:43:20@

    __exported by:__ @rpm\/rpmsw.h@
    -}
  , rpmop_s_count :: FC.CInt
    {- ^ Number of operations.

    __C declaration:__ @count@

    __defined at:__ @rpm\/rpmsw.h:44:11@

    __exported by:__ @rpm\/rpmsw.h@
    -}
  , rpmop_s_bytes :: HsBindgen.Runtime.Prelude.CSize
    {- ^ Number of bytes transferred.

    __C declaration:__ @bytes@

    __defined at:__ @rpm\/rpmsw.h:45:13@

    __exported by:__ @rpm\/rpmsw.h@
    -}
  , rpmop_s_usecs :: Rpmtime_t
    {- ^ Number of ticks.

    __C declaration:__ @usecs@

    __defined at:__ @rpm\/rpmsw.h:46:16@

    __exported by:__ @rpm\/rpmsw.h@
    -}
  }

instance F.Storable Rpmop_s where

  sizeOf = \_ -> (40 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Rpmop_s
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)
      <*> F.peekByteOff ptr0 (24 :: Int)
      <*> F.peekByteOff ptr0 (32 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Rpmop_s rpmop_s_begin2 rpmop_s_count3 rpmop_s_bytes4 rpmop_s_usecs5 ->
               F.pokeByteOff ptr0 (0 :: Int) rpmop_s_begin2
            >> F.pokeByteOff ptr0 (16 :: Int) rpmop_s_count3
            >> F.pokeByteOff ptr0 (24 :: Int) rpmop_s_bytes4
            >> F.pokeByteOff ptr0 (32 :: Int) rpmop_s_usecs5
