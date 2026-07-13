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
    ( Example.A(..)
    , Example.Bool'(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @macro A@

    __defined at:__ @types\/primitives\/bool_typedef_override.h 4:9@

    __exported by:__ @types\/primitives\/bool_typedef_override.h@
-}
newtype A = A
  { unwrapA :: BG.CInt
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

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapA" A ty where

  hasField =
    \x0 ->
      (\y1 -> A {unwrapA = y1}, BG.getField @"unwrapA" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrapA" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @bool@

    __defined at:__ @types\/primitives\/bool_typedef_override.h 5:13@

    __exported by:__ @types\/primitives\/bool_typedef_override.h@
-}
newtype Bool' = Bool'
  { unwrapBool' :: BG.CInt
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

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "unwrapBool'" Bool' ty where

  hasField =
    \x0 ->
      (\y1 ->
         Bool' {unwrapBool' = y1}, BG.getField @"unwrapBool'" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapBool'" (BG.Ptr Bool') (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapBool'")

instance HasCField.HasCField Bool' "unwrapBool'" where

  type CFieldType Bool' "unwrapBool'" = BG.CInt

  offset# = \_ -> \_ -> 0
