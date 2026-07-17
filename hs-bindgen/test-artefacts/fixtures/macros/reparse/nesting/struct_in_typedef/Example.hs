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
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @macro MyInt@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_typedef.h 1:9@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_typedef.h@
-}
newtype MyInt = MyInt
  { unwrapMyInt :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapMyInt" MyInt ty where

  hasField =
    \x0 ->
      (\y1 ->
         MyInt {unwrapMyInt = y1}, BG.getField @"unwrapMyInt" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapMyInt" (BG.Ptr MyInt) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMyInt")

instance HasCField.HasCField MyInt "unwrapMyInt" where

  type CFieldType MyInt "unwrapMyInt" = BG.CInt

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
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize T1 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T1 where

  readRaw =
    \ptr0 ->
          pure T1
      <*> HasCField.readRaw (BG.Proxy @"t1_x") ptr0

instance Marshal.WriteRaw T1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T1 t1_x2 ->
            HasCField.writeRaw (BG.Proxy @"t1_x") ptr0 t1_x2

deriving via Marshal.EquivStorable T1 instance BG.Storable T1

instance (ty ~ MyInt) => BG.CompatHasField.HasField "t1_x" T1 ty where

  hasField =
    \x0 ->
      (\y1 -> T1 {t1_x = y1}, BG.getField @"t1_x" x0)

instance (ty ~ MyInt) => BG.HasField "t1_x" (BG.Ptr T1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t1_x")

instance HasCField.HasCField T1 "t1_x" where

  type CFieldType T1 "t1_x" = MyInt

  offset# = \_ -> \_ -> 0

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
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize T2_Aux where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T2_Aux where

  readRaw =
    \ptr0 ->
          pure T2_Aux
      <*> HasCField.readRaw (BG.Proxy @"t2_Aux_x") ptr0

instance Marshal.WriteRaw T2_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T2_Aux t2_Aux_x2 ->
            HasCField.writeRaw (BG.Proxy @"t2_Aux_x") ptr0 t2_Aux_x2

deriving via Marshal.EquivStorable T2_Aux instance BG.Storable T2_Aux

instance (ty ~ MyInt) => BG.CompatHasField.HasField "t2_Aux_x" T2_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         T2_Aux {t2_Aux_x = y1}, BG.getField @"t2_Aux_x" x0)

instance ( ty ~ MyInt
         ) => BG.HasField "t2_Aux_x" (BG.Ptr T2_Aux) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t2_Aux_x")

instance HasCField.HasCField T2_Aux "t2_Aux_x" where

  type CFieldType T2_Aux "t2_Aux_x" = MyInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T2@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_typedef.h 4:32@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_typedef.h@
-}
newtype T2 = T2
  { unwrapT2 :: BG.Ptr T2_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr T2_Aux
         ) => BG.CompatHasField.HasField "unwrapT2" T2 ty where

  hasField =
    \x0 ->
      (\y1 ->
         T2 {unwrapT2 = y1}, BG.getField @"unwrapT2" x0)

instance ( ty ~ BG.Ptr T2_Aux
         ) => BG.HasField "unwrapT2" (BG.Ptr T2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT2")

instance HasCField.HasCField T2 "unwrapT2" where

  type CFieldType T2 "unwrapT2" = BG.Ptr T2_Aux

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
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize T3_Aux where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T3_Aux where

  readRaw =
    \ptr0 ->
          pure T3_Aux
      <*> HasCField.readRaw (BG.Proxy @"t3_Aux_x") ptr0

instance Marshal.WriteRaw T3_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T3_Aux t3_Aux_x2 ->
            HasCField.writeRaw (BG.Proxy @"t3_Aux_x") ptr0 t3_Aux_x2

deriving via Marshal.EquivStorable T3_Aux instance BG.Storable T3_Aux

instance (ty ~ MyInt) => BG.CompatHasField.HasField "t3_Aux_x" T3_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         T3_Aux {t3_Aux_x = y1}, BG.getField @"t3_Aux_x" x0)

instance ( ty ~ MyInt
         ) => BG.HasField "t3_Aux_x" (BG.Ptr T3_Aux) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t3_Aux_x")

instance HasCField.HasCField T3_Aux "t3_Aux_x" where

  type CFieldType T3_Aux "t3_Aux_x" = MyInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T3@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_typedef.h 5:32@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_typedef.h@
-}
newtype T3 = T3
  { unwrapT3 :: BG.Ptr (BG.Ptr T3_Aux)
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr (BG.Ptr T3_Aux)
         ) => BG.CompatHasField.HasField "unwrapT3" T3 ty where

  hasField =
    \x0 ->
      (\y1 ->
         T3 {unwrapT3 = y1}, BG.getField @"unwrapT3" x0)

instance ( ty ~ BG.Ptr (BG.Ptr T3_Aux)
         ) => BG.HasField "unwrapT3" (BG.Ptr T3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT3")

instance HasCField.HasCField T3 "unwrapT3" where

  type CFieldType T3 "unwrapT3" =
    BG.Ptr (BG.Ptr T3_Aux)

  offset# = \_ -> \_ -> 0
