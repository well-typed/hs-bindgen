{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @MY_TYPE@

    __defined at:__ @macros\/macro_typedef_struct.h 1:9@

    __exported by:__ @macros\/macro_typedef_struct.h@
-}
newtype MY_TYPE = MY_TYPE
  { unwrapMY_TYPE :: RIP.CInt
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

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unwrapMY_TYPE" (RIP.Ptr MY_TYPE) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMY_TYPE")

instance HasCField.HasCField MY_TYPE "unwrapMY_TYPE" where

  type CFieldType MY_TYPE "unwrapMY_TYPE" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct bar@

    __defined at:__ @macros\/macro_typedef_struct.h 3:9@

    __exported by:__ @macros\/macro_typedef_struct.h@
-}
data Bar = Bar
  { bar_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/macro_typedef_struct.h 4:7@

         __exported by:__ @macros\/macro_typedef_struct.h@
    -}
  , bar_y :: MY_TYPE
    {- ^ __C declaration:__ @y@

         __defined at:__ @macros\/macro_typedef_struct.h 5:11@

         __exported by:__ @macros\/macro_typedef_struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bar where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Bar where

  readRaw =
    \ptr0 ->
          pure Bar
      <*> HasCField.readRaw (RIP.Proxy @"bar_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"bar_y") ptr0

instance Marshal.WriteRaw Bar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_x2 bar_y3 ->
               HasCField.writeRaw (RIP.Proxy @"bar_x") ptr0 bar_x2
            >> HasCField.writeRaw (RIP.Proxy @"bar_y") ptr0 bar_y3

deriving via Marshal.EquivStorable Bar instance RIP.Storable Bar

instance HasCField.HasCField Bar "bar_x" where

  type CFieldType Bar "bar_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "bar_x" (RIP.Ptr Bar) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"bar_x")

instance HasCField.HasCField Bar "bar_y" where

  type CFieldType Bar "bar_y" = MY_TYPE

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) MY_TYPE
         ) => RIP.HasField "bar_y" (RIP.Ptr Bar) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"bar_y")
