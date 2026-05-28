{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.MyInt(..)
    , Example.T1(..)
    , Example.T2_Aux(..)
    , Example.T2(..)
    , Example.T3_Aux(..)
    , Example.T3(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @macro MyInt@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_typedef.h 1:9@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_typedef.h@
-}
newtype MyInt = MyInt
  { unwrapMyInt :: RIP.CInt
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
         ) => RIP.HasField "unwrapMyInt" (RIP.Ptr MyInt) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMyInt")

instance HasCField.HasCField MyInt "unwrapMyInt" where

  type CFieldType MyInt "unwrapMyInt" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct T1@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_typedef.h 3:9@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_typedef.h@
-}
data T1 = T1
  { t1_x :: MyInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting\/struct_in_typedef.h 3:24@

         __exported by:__ @macros\/reparse\/nesting\/struct_in_typedef.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize T1 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T1 where

  readRaw =
    \ptr0 ->
          pure T1
      <*> HasCField.readRaw (RIP.Proxy @"t1_x") ptr0

instance Marshal.WriteRaw T1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T1 t1_x2 ->
            HasCField.writeRaw (RIP.Proxy @"t1_x") ptr0 t1_x2

deriving via Marshal.EquivStorable T1 instance RIP.Storable T1

instance HasCField.HasCField T1 "t1_x" where

  type CFieldType T1 "t1_x" = MyInt

  offset# = \_ -> \_ -> 0

instance (ty ~ MyInt) => RIP.HasField "t1_x" (RIP.Ptr T1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"t1_x")

{-| __C declaration:__ @struct \@T2_Aux@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_typedef.h 4:9@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_typedef.h@
-}
data T2_Aux = T2_Aux
  { t2_Aux_x :: MyInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting\/struct_in_typedef.h 4:24@

         __exported by:__ @macros\/reparse\/nesting\/struct_in_typedef.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize T2_Aux where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T2_Aux where

  readRaw =
    \ptr0 ->
          pure T2_Aux
      <*> HasCField.readRaw (RIP.Proxy @"t2_Aux_x") ptr0

instance Marshal.WriteRaw T2_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T2_Aux t2_Aux_x2 ->
            HasCField.writeRaw (RIP.Proxy @"t2_Aux_x") ptr0 t2_Aux_x2

deriving via Marshal.EquivStorable T2_Aux instance RIP.Storable T2_Aux

instance HasCField.HasCField T2_Aux "t2_Aux_x" where

  type CFieldType T2_Aux "t2_Aux_x" = MyInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ MyInt
         ) => RIP.HasField "t2_Aux_x" (RIP.Ptr T2_Aux) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"t2_Aux_x")

{-| __C declaration:__ @T2@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_typedef.h 4:32@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_typedef.h@
-}
newtype T2 = T2
  { unwrapT2 :: RIP.Ptr T2_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ RIP.Ptr T2_Aux
         ) => RIP.HasField "unwrapT2" (RIP.Ptr T2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapT2")

instance HasCField.HasCField T2 "unwrapT2" where

  type CFieldType T2 "unwrapT2" = RIP.Ptr T2_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@T3_Aux@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_typedef.h 5:9@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_typedef.h@
-}
data T3_Aux = T3_Aux
  { t3_Aux_x :: MyInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting\/struct_in_typedef.h 5:24@

         __exported by:__ @macros\/reparse\/nesting\/struct_in_typedef.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize T3_Aux where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T3_Aux where

  readRaw =
    \ptr0 ->
          pure T3_Aux
      <*> HasCField.readRaw (RIP.Proxy @"t3_Aux_x") ptr0

instance Marshal.WriteRaw T3_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T3_Aux t3_Aux_x2 ->
            HasCField.writeRaw (RIP.Proxy @"t3_Aux_x") ptr0 t3_Aux_x2

deriving via Marshal.EquivStorable T3_Aux instance RIP.Storable T3_Aux

instance HasCField.HasCField T3_Aux "t3_Aux_x" where

  type CFieldType T3_Aux "t3_Aux_x" = MyInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ MyInt
         ) => RIP.HasField "t3_Aux_x" (RIP.Ptr T3_Aux) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"t3_Aux_x")

{-| __C declaration:__ @T3@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_typedef.h 5:32@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_typedef.h@
-}
newtype T3 = T3
  { unwrapT3 :: RIP.Ptr (RIP.Ptr T3_Aux)
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ RIP.Ptr (RIP.Ptr T3_Aux)
         ) => RIP.HasField "unwrapT3" (RIP.Ptr T3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapT3")

instance HasCField.HasCField T3 "unwrapT3" where

  type CFieldType T3 "unwrapT3" =
    RIP.Ptr (RIP.Ptr T3_Aux)

  offset# = \_ -> \_ -> 0
