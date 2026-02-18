{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Foreign
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @uint32_t@

    __defined at:__ @bits\/alltypes.h 131:25@

    __exported by:__ @program-analysis\/program_slicing_simple.h@
-}
newtype Uint32_t = Uint32_t
  { unwrapUint32_t :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.Bitfield
    , RIP.Bits
    , Bounded
    , Enum
    , RIP.FiniteBits
    , RIP.HasFFIType
    , Integral
    , RIP.Ix
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrapUint32_t" (RIP.Ptr Uint32_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapUint32_t")

instance HasCField.HasCField Uint32_t "unwrapUint32_t" where

  type CFieldType Uint32_t "unwrapUint32_t" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct foo@

    __defined at:__ @program-analysis\/program_slicing_simple.h 3:8@

    __exported by:__ @program-analysis\/program_slicing_simple.h@
-}
data Foo = Foo
  { foo_sixty_four :: Foreign.Word64
    {- ^ __C declaration:__ @sixty_four@

         __defined at:__ @program-analysis\/program_slicing_simple.h 4:12@

         __exported by:__ @program-analysis\/program_slicing_simple.h@
    -}
  , foo_thirty_two :: Uint32_t
    {- ^ __C declaration:__ @thirty_two@

         __defined at:__ @program-analysis\/program_slicing_simple.h 5:12@

         __exported by:__ @program-analysis\/program_slicing_simple.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance RIP.Storable Foo where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Foo
      <*> HasCField.peek (RIP.Proxy @"foo_sixty_four") ptr0
      <*> HasCField.peek (RIP.Proxy @"foo_thirty_two") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_sixty_four2 foo_thirty_two3 ->
               HasCField.poke (RIP.Proxy @"foo_sixty_four") ptr0 foo_sixty_four2
            >> HasCField.poke (RIP.Proxy @"foo_thirty_two") ptr0 foo_thirty_two3

instance HasCField.HasCField Foo "foo_sixty_four" where

  type CFieldType Foo "foo_sixty_four" = Foreign.Word64

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Foreign.Word64
         ) => RIP.HasField "foo_sixty_four" (RIP.Ptr Foo) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"foo_sixty_four")

instance HasCField.HasCField Foo "foo_thirty_two" where

  type CFieldType Foo "foo_thirty_two" = Uint32_t

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) Uint32_t
         ) => RIP.HasField "foo_thirty_two" (RIP.Ptr Foo) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"foo_thirty_two")
