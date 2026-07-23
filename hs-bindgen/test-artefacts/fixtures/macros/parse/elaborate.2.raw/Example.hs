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
    ( Example.iNNER_A
    , Example.iNNER_B
    , Example.oUTER_A
    , Example.Outer_int(..)
    , Example.Inner_int(..)
    , Example.oUTER_B
    , Example.oUTER_C
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @macro INNER_A@

    __defined at:__ @elaborate_inner.h 1:9@

    __exported by:__ @macros\/parse\/elaborate.h@
-}
iNNER_A :: [String]
iNNER_A = ["OUTER_A"]

{-| __C declaration:__ @macro INNER_B@

    __defined at:__ @elaborate_inner.h 2:9@

    __exported by:__ @macros\/parse\/elaborate.h@
-}
iNNER_B :: [String]
iNNER_B = ["2"]

{-| __C declaration:__ @macro OUTER_A@

    __defined at:__ @macros\/parse\/elaborate.h 7:9@

    __exported by:__ @macros\/parse\/elaborate.h@
-}
oUTER_A :: [String]
oUTER_A = ["1"]

{-| __C declaration:__ @outer_int@

    __defined at:__ @macros\/parse\/elaborate.h 8:13@

    __exported by:__ @macros\/parse\/elaborate.h@
-}
newtype Outer_int = Outer_int
  { unwrapOuter_int :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapOuter_int" Outer_int ty where

  hasField =
    \x0 ->
      (\y1 ->
         Outer_int {unwrapOuter_int = y1}, BG.getField @"unwrapOuter_int" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapOuter_int" (BG.Ptr Outer_int) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapOuter_int")

instance HasCField.HasCField Outer_int "unwrapOuter_int" where

  type CFieldType Outer_int "unwrapOuter_int" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @inner_int@

    __defined at:__ @elaborate_inner.h 3:19@

    __exported by:__ @macros\/parse\/elaborate.h@
-}
newtype Inner_int = Inner_int
  { unwrapInner_int :: Outer_int
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

instance ( ty ~ Outer_int
         ) => BG.CompatHasField.HasField "unwrapInner_int" Inner_int ty where

  hasField =
    \x0 ->
      (\y1 ->
         Inner_int {unwrapInner_int = y1}, BG.getField @"unwrapInner_int" x0)

instance ( ty ~ Outer_int
         ) => BG.HasField "unwrapInner_int" (BG.Ptr Inner_int) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapInner_int")

instance HasCField.HasCField Inner_int "unwrapInner_int" where

  type CFieldType Inner_int "unwrapInner_int" =
    Outer_int

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro OUTER_B@

    __defined at:__ @macros\/parse\/elaborate.h 12:9@

    __exported by:__ @macros\/parse\/elaborate.h@
-}
oUTER_B :: [String]
oUTER_B = ["INNER_A"]

{-| __C declaration:__ @macro OUTER_C@

    __defined at:__ @macros\/parse\/elaborate.h 13:9@

    __exported by:__ @macros\/parse\/elaborate.h@
-}
oUTER_C :: [String]
oUTER_C = ["INNER_B"]
