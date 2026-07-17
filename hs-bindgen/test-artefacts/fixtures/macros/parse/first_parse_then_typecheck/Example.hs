{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
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
    ( Example.valueA
    , Example.valueB
    , Example.TypeA(..)
    , Example.TypeB(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @macro ValueA@

    __defined at:__ @macros\/parse\/first_parse_then_typecheck.h 7:9@

    __exported by:__ @macros\/parse\/first_parse_then_typecheck.h@
-}
valueA :: BG.CInt
valueA = (1 :: BG.CInt)

{-| __C declaration:__ @macro ValueB@

    __defined at:__ @macros\/parse\/first_parse_then_typecheck.h 6:9@

    __exported by:__ @macros\/parse\/first_parse_then_typecheck.h@
-}
valueB :: BG.CInt
valueB = valueA

{-| __C declaration:__ @macro TypeA@

    __defined at:__ @macros\/parse\/first_parse_then_typecheck.h 10:9@

    __exported by:__ @macros\/parse\/first_parse_then_typecheck.h@
-}
newtype TypeA = TypeA
  { unwrapTypeA :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapTypeA" TypeA ty where

  hasField =
    \x0 ->
      (\y1 ->
         TypeA {unwrapTypeA = y1}, BG.getField @"unwrapTypeA" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapTypeA" (BG.Ptr TypeA) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapTypeA")

instance HasCField.HasCField TypeA "unwrapTypeA" where

  type CFieldType TypeA "unwrapTypeA" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro TypeB@

    __defined at:__ @macros\/parse\/first_parse_then_typecheck.h 9:9@

    __exported by:__ @macros\/parse\/first_parse_then_typecheck.h@
-}
newtype TypeB = TypeB
  { unwrapTypeB :: TypeA
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

instance (ty ~ TypeA) => BG.CompatHasField.HasField "unwrapTypeB" TypeB ty where

  hasField =
    \x0 ->
      (\y1 ->
         TypeB {unwrapTypeB = y1}, BG.getField @"unwrapTypeB" x0)

instance ( ty ~ TypeA
         ) => BG.HasField "unwrapTypeB" (BG.Ptr TypeB) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapTypeB")

instance HasCField.HasCField TypeB "unwrapTypeB" where

  type CFieldType TypeB "unwrapTypeB" = TypeA

  offset# = \_ -> \_ -> 0
