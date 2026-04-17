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
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @OUTER_BEFORE_CIRCULAR_INCLUDE@

    __defined at:__ @program-analysis\/circular_includes.h 2:13@

    __exported by:__ @program-analysis\/circular_includes.h@, @program-analysis\/circular_includes.h@
-}
newtype OUTER_BEFORE_CIRCULAR_INCLUDE = OUTER_BEFORE_CIRCULAR_INCLUDE
  { unwrapOUTER_BEFORE_CIRCULAR_INCLUDE :: RIP.CInt
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

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "unwrapOUTER_BEFORE_CIRCULAR_INCLUDE" (RIP.Ptr OUTER_BEFORE_CIRCULAR_INCLUDE) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapOUTER_BEFORE_CIRCULAR_INCLUDE")

instance HasCField.HasCField OUTER_BEFORE_CIRCULAR_INCLUDE "unwrapOUTER_BEFORE_CIRCULAR_INCLUDE" where

  type CFieldType OUTER_BEFORE_CIRCULAR_INCLUDE "unwrapOUTER_BEFORE_CIRCULAR_INCLUDE" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @OUTER_AFTER_CIRCULAR_INCLUDE@

    __defined at:__ @program-analysis\/circular_includes.h 4:13@

    __exported by:__ @program-analysis\/circular_includes.h@, @program-analysis\/circular_includes.h@
-}
newtype OUTER_AFTER_CIRCULAR_INCLUDE = OUTER_AFTER_CIRCULAR_INCLUDE
  { unwrapOUTER_AFTER_CIRCULAR_INCLUDE :: RIP.CInt
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

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "unwrapOUTER_AFTER_CIRCULAR_INCLUDE" (RIP.Ptr OUTER_AFTER_CIRCULAR_INCLUDE) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapOUTER_AFTER_CIRCULAR_INCLUDE")

instance HasCField.HasCField OUTER_AFTER_CIRCULAR_INCLUDE "unwrapOUTER_AFTER_CIRCULAR_INCLUDE" where

  type CFieldType OUTER_AFTER_CIRCULAR_INCLUDE "unwrapOUTER_AFTER_CIRCULAR_INCLUDE" =
    RIP.CInt

  offset# = \_ -> \_ -> 0
