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
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.myInt
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
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @macro MyInt@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 1:9@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
-}
myInt :: [String]
myInt = ["int"]

{-| __C declaration:__ @union T1@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 3:9@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
-}
newtype T1 = T1
  { unwrapT1 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize T1

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw T1

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw T1

deriving via Marshal.EquivStorable T1 instance BG.Storable T1

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion T1

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 3:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "t1_x" T1 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 3:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "t1_x" T1 ty where

  hasField =
    \x0 -> (BG.setUnionPayload, BG.getField @"t1_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "t1_x" (BG.Ptr T1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t1_x")

instance HasCField.HasCField T1 "t1_x" where

  type CFieldType T1 "t1_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@T2_Aux@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 4:9@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
-}
newtype T2_Aux = T2_Aux
  { unwrapT2_Aux :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize T2_Aux

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw T2_Aux

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw T2_Aux

deriving via Marshal.EquivStorable T2_Aux instance BG.Storable T2_Aux

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion T2_Aux

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 4:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "t2_Aux_x" T2_Aux ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 4:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "t2_Aux_x" T2_Aux ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"t2_Aux_x" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "t2_Aux_x" (BG.Ptr T2_Aux) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t2_Aux_x")

instance HasCField.HasCField T2_Aux "t2_Aux_x" where

  type CFieldType T2_Aux "t2_Aux_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T2@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 4:31@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
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

{-| __C declaration:__ @union \@T3_Aux@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 5:9@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
-}
newtype T3_Aux = T3_Aux
  { unwrapT3_Aux :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize T3_Aux

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw T3_Aux

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw T3_Aux

deriving via Marshal.EquivStorable T3_Aux instance BG.Storable T3_Aux

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion T3_Aux

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 5:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "t3_Aux_x" T3_Aux ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 5:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "t3_Aux_x" T3_Aux ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"t3_Aux_x" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "t3_Aux_x" (BG.Ptr T3_Aux) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t3_Aux_x")

instance HasCField.HasCField T3_Aux "t3_Aux_x" where

  type CFieldType T3_Aux "t3_Aux_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T3@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 5:31@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
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
