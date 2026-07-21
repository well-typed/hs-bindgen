{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Uint32_t(..)
    , Example.Foo(..)
    )
  where

import qualified Foreign
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @uint32_t@

    __defined at:__ @bits\/alltypes.h 131:25@

    __exported by:__ @program-analysis\/program_slicing_simple.h@
-}
newtype Uint32_t = Uint32_t
  { unwrapUint32_t :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unwrapUint32_t" Uint32_t ty where

  hasField =
    \x0 ->
      (\y1 ->
         Uint32_t {unwrapUint32_t = y1}, BG.getField @"unwrapUint32_t" x0)

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapUint32_t" (BG.Ptr Uint32_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapUint32_t")

instance HasCField.HasCField Uint32_t "unwrapUint32_t" where

  type CFieldType Uint32_t "unwrapUint32_t" = BG.CUInt

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
  deriving stock (Eq, BG.Generic, Show)

instance BG.Storable Foo where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Foo
      <*> HasCField.peek (BG.Proxy @"foo_sixty_four") ptr0
      <*> HasCField.peek (BG.Proxy @"foo_thirty_two") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_sixty_four2 foo_thirty_two3 ->
               HasCField.poke (BG.Proxy @"foo_sixty_four") ptr0 foo_sixty_four2
            >> HasCField.poke (BG.Proxy @"foo_thirty_two") ptr0 foo_thirty_two3

{-| __C declaration:__ @sixty_four@

    __defined at:__ @program-analysis\/program_slicing_simple.h 4:12@

    __exported by:__ @program-analysis\/program_slicing_simple.h@
-}
instance ( ty ~ Foreign.Word64
         ) => BG.CompatHasField.HasField "foo_sixty_four" Foo ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Foo {foo_sixty_four = y1, foo_thirty_two = BG.getField @"foo_thirty_two" x0}
      , BG.getField @"foo_sixty_four" x0
      )

instance ( ty ~ Foreign.Word64
         ) => BG.HasField "foo_sixty_four" (BG.Ptr Foo) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"foo_sixty_four")

instance HasCField.HasCField Foo "foo_sixty_four" where

  type CFieldType Foo "foo_sixty_four" = Foreign.Word64

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @thirty_two@

    __defined at:__ @program-analysis\/program_slicing_simple.h 5:12@

    __exported by:__ @program-analysis\/program_slicing_simple.h@
-}
instance ( ty ~ Uint32_t
         ) => BG.CompatHasField.HasField "foo_thirty_two" Foo ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Foo {foo_thirty_two = y1, foo_sixty_four = BG.getField @"foo_sixty_four" x0}
      , BG.getField @"foo_thirty_two" x0
      )

instance ( ty ~ Uint32_t
         ) => BG.HasField "foo_thirty_two" (BG.Ptr Foo) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"foo_thirty_two")

instance HasCField.HasCField Foo "foo_thirty_two" where

  type CFieldType Foo "foo_thirty_two" = Uint32_t

  offset# = \_ -> \_ -> 8
