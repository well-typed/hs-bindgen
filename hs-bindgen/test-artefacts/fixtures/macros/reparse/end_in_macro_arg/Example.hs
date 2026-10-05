{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
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
    ( Example.T(..)
    , Example.pARAMS
    , Example.iD
    , Example.A(..)
    , Example.S(..)
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @macro T@

    __defined at:__ @macros\/reparse\/end_in_macro_arg.h 8:9@

    __exported by:__ @macros\/reparse\/end_in_macro_arg.h@
-}
newtype T = T
  { unwrapT :: BG.CInt
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

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapT" T ty where

  hasField =
    \x0 ->
      (\y1 -> T {unwrapT = y1}, BG.getField @"unwrapT" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrapT" (BG.Ptr T) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT")

instance HasCField.HasCField T "unwrapT" where

  type CFieldType T "unwrapT" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro PARAMS@

    __defined at:__ @macros\/reparse\/end_in_macro_arg.h 9:9@

    __exported by:__ @macros\/reparse\/end_in_macro_arg.h@
-}
pARAMS :: forall a0. a0 -> a0
pARAMS = \args0 -> args0

{-| __C declaration:__ @macro ID@

    __defined at:__ @macros\/reparse\/end_in_macro_arg.h 10:9@

    __exported by:__ @macros\/reparse\/end_in_macro_arg.h@
-}
iD :: forall a0. a0 -> a0
iD = \x0 -> x0

{-| __C declaration:__ @A@

    __defined at:__ @macros\/reparse\/end_in_macro_arg.h 14:11@

    __exported by:__ @macros\/reparse\/end_in_macro_arg.h@
-}
newtype A = A
  { unwrapA :: CA.ConstantArray 3 T
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ CA.ConstantArray 3 T
         ) => BG.CompatHasField.HasField "unwrapA" A ty where

  hasField =
    \x0 ->
      (\y1 -> A {unwrapA = y1}, BG.getField @"unwrapA" x0)

instance ( ty ~ CA.ConstantArray 3 T
         ) => BG.HasField "unwrapA" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = CA.ConstantArray 3 T

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct S@

    __defined at:__ @macros\/reparse\/end_in_macro_arg.h 16:8@

    __exported by:__ @macros\/reparse\/end_in_macro_arg.h@
-}
data S = S
  { s_x :: CA.ConstantArray 3 T
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/end_in_macro_arg.h 16:14@

         __exported by:__ @macros\/reparse\/end_in_macro_arg.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S where

  readRaw =
    \ptr0 ->
          pure S
      <*> HasCField.readRaw (BG.Proxy @"s_x") ptr0

instance Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_x2 ->
            HasCField.writeRaw (BG.Proxy @"s_x") ptr0 s_x2

deriving via Marshal.EquivStorable S instance BG.Storable S

deriving via Struct.IsStructViaReadRaw S instance Struct.IsStruct S

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/end_in_macro_arg.h 16:14@

    __exported by:__ @macros\/reparse\/end_in_macro_arg.h@
-}
instance ( ty ~ CA.ConstantArray 3 T
         ) => BG.CompatHasField.HasField "s_x" S ty where

  hasField =
    \x0 -> (\y1 -> S {s_x = y1}, BG.getField @"s_x" x0)

instance ( ty ~ CA.ConstantArray 3 T
         ) => BG.HasField "s_x" (BG.Ptr S) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s_x")

instance HasCField.HasCField S "s_x" where

  type CFieldType S "s_x" = CA.ConstantArray 3 T

  offset# = \_ -> \_ -> 0
