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
    ( Example.OUTER_BEFORE_CIRCULAR_INCLUDE(..)
    , Example.OUTER_AFTER_CIRCULAR_INCLUDE(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @OUTER_BEFORE_CIRCULAR_INCLUDE@

    __defined at:__ @program-analysis\/circular_includes.h 2:13@

    __exported by:__ @program-analysis\/circular_includes.h@
-}
newtype OUTER_BEFORE_CIRCULAR_INCLUDE = OUTER_BEFORE_CIRCULAR_INCLUDE
  { unwrapOUTER_BEFORE_CIRCULAR_INCLUDE :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapOUTER_BEFORE_CIRCULAR_INCLUDE" OUTER_BEFORE_CIRCULAR_INCLUDE ty where

  hasField =
    \x0 ->
      ( \y1 ->
          OUTER_BEFORE_CIRCULAR_INCLUDE {unwrapOUTER_BEFORE_CIRCULAR_INCLUDE = y1}
      , BG.getField @"unwrapOUTER_BEFORE_CIRCULAR_INCLUDE" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapOUTER_BEFORE_CIRCULAR_INCLUDE" (BG.Ptr OUTER_BEFORE_CIRCULAR_INCLUDE) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapOUTER_BEFORE_CIRCULAR_INCLUDE")

instance HasCField.HasCField OUTER_BEFORE_CIRCULAR_INCLUDE "unwrapOUTER_BEFORE_CIRCULAR_INCLUDE" where

  type CFieldType OUTER_BEFORE_CIRCULAR_INCLUDE "unwrapOUTER_BEFORE_CIRCULAR_INCLUDE" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @OUTER_AFTER_CIRCULAR_INCLUDE@

    __defined at:__ @program-analysis\/circular_includes.h 4:13@

    __exported by:__ @program-analysis\/circular_includes.h@
-}
newtype OUTER_AFTER_CIRCULAR_INCLUDE = OUTER_AFTER_CIRCULAR_INCLUDE
  { unwrapOUTER_AFTER_CIRCULAR_INCLUDE :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapOUTER_AFTER_CIRCULAR_INCLUDE" OUTER_AFTER_CIRCULAR_INCLUDE ty where

  hasField =
    \x0 ->
      ( \y1 ->
          OUTER_AFTER_CIRCULAR_INCLUDE {unwrapOUTER_AFTER_CIRCULAR_INCLUDE = y1}
      , BG.getField @"unwrapOUTER_AFTER_CIRCULAR_INCLUDE" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapOUTER_AFTER_CIRCULAR_INCLUDE" (BG.Ptr OUTER_AFTER_CIRCULAR_INCLUDE) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapOUTER_AFTER_CIRCULAR_INCLUDE")

instance HasCField.HasCField OUTER_AFTER_CIRCULAR_INCLUDE "unwrapOUTER_AFTER_CIRCULAR_INCLUDE" where

  type CFieldType OUTER_AFTER_CIRCULAR_INCLUDE "unwrapOUTER_AFTER_CIRCULAR_INCLUDE" =
    BG.CInt

  offset# = \_ -> \_ -> 0
