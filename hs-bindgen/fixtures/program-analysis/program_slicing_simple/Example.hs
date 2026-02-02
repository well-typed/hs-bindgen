{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.Bitfield
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Marshal
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @uint32_t@

    __defined at:__ @bits\/alltypes.h 131:25@

    __exported by:__ @program-analysis\/program_slicing_simple.h@
-}
newtype Uint32_t = Uint32_t
  { unwrapUint32_t :: FC.CUInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Uint32_t) "unwrapUint32_t")
         ) => GHC.Records.HasField "unwrapUint32_t" (Ptr.Ptr Uint32_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapUint32_t")

instance HsBindgen.Runtime.HasCField.HasCField Uint32_t "unwrapUint32_t" where

  type CFieldType Uint32_t "unwrapUint32_t" = FC.CUInt

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
  deriving stock (Eq, Show)

instance F.Storable Foo where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Foo
      <*> HsBindgen.Runtime.HasCField.peek (Data.Proxy.Proxy @"foo_sixty_four") ptr0
      <*> HsBindgen.Runtime.HasCField.peek (Data.Proxy.Proxy @"foo_thirty_two") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_sixty_four2 foo_thirty_two3 ->
               HsBindgen.Runtime.HasCField.poke (Data.Proxy.Proxy @"foo_sixty_four") ptr0 foo_sixty_four2
            >> HsBindgen.Runtime.HasCField.poke (Data.Proxy.Proxy @"foo_thirty_two") ptr0 foo_thirty_two3

instance HsBindgen.Runtime.HasCField.HasCField Foo "foo_sixty_four" where

  type CFieldType Foo "foo_sixty_four" = Foreign.Word64

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Foo) "foo_sixty_four")
         ) => GHC.Records.HasField "foo_sixty_four" (Ptr.Ptr Foo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"foo_sixty_four")

instance HsBindgen.Runtime.HasCField.HasCField Foo "foo_thirty_two" where

  type CFieldType Foo "foo_thirty_two" = Uint32_t

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Foo) "foo_thirty_two")
         ) => GHC.Records.HasField "foo_thirty_two" (Ptr.Ptr Foo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"foo_thirty_two")
