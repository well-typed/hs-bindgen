{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.SSS_anon'anon'x_anon'x(..)
    , Example.SSS_anon'anon'x(..)
    , Example.SSS(..)
    , Example.USS_anon'anon'x_anon'x(..)
    , Example.USS_anon'anon'x(..)
    , Example.USS(..)
    , Example.SUS_anon'anon'x_anon'x(..)
    , Example.SUS_anon'anon'x(..)
    , Example.SUS(..)
    , Example.UUS_anon'anon'x_anon'x(..)
    , Example.UUS_anon'anon'x(..)
    , Example.UUS(..)
    , Example.SSU_anon'anon'x_anon'x(..)
    , Example.SSU_anon'anon'x(..)
    , Example.SSU(..)
    , Example.USU_anon'anon'x_anon'x(..)
    , Example.USU_anon'anon'x(..)
    , Example.USU(..)
    , Example.SUU_anon'anon'x_anon'x(..)
    , Example.SUU_anon'anon'x(..)
    , Example.SUU(..)
    , Example.UUU_anon'anon'x_anon'x(..)
    , Example.UUU_anon'anon'x(..)
    , Example.UUU(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @struct \@SSS_anon\'anon\'x_anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 17:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SSS_anon'anon'x_anon'x = SSS_anon'anon'x_anon'x
  { sSS_anon'anon'x_anon'x_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 18:11@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize SSS_anon'anon'x_anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SSS_anon'anon'x_anon'x where

  readRaw =
    \ptr0 ->
          pure SSS_anon'anon'x_anon'x
      <*> HasCField.readRaw (BG.Proxy @"sSS_anon'anon'x_anon'x_x") ptr0

instance Marshal.WriteRaw SSS_anon'anon'x_anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SSS_anon'anon'x_anon'x sSS_anon'anon'x_anon'x_x2 ->
            HasCField.writeRaw (BG.Proxy @"sSS_anon'anon'x_anon'x_x") ptr0 sSS_anon'anon'x_anon'x_x2

deriving via Marshal.EquivStorable SSS_anon'anon'x_anon'x instance BG.Storable SSS_anon'anon'x_anon'x

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 18:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "sSS_anon'anon'x_anon'x_x" SSS_anon'anon'x_anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          SSS_anon'anon'x_anon'x {sSS_anon'anon'x_anon'x_x = y1}
      , BG.getField @"sSS_anon'anon'x_anon'x_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "sSS_anon'anon'x_anon'x_x" (BG.Ptr SSS_anon'anon'x_anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"sSS_anon'anon'x_anon'x_x")

instance HasCField.HasCField SSS_anon'anon'x_anon'x "sSS_anon'anon'x_anon'x_x" where

  type CFieldType SSS_anon'anon'x_anon'x "sSS_anon'anon'x_anon'x_x" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@SSS_anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 16:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SSS_anon'anon'x = SSS_anon'anon'x
  { sSS_anon'anon'x_anon'x :: SSS_anon'anon'x_anon'x
    {- ^ __C declaration:__ @anon\'x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 17:5@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize SSS_anon'anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SSS_anon'anon'x where

  readRaw =
    \ptr0 ->
          pure SSS_anon'anon'x
      <*> HasCField.readRaw (BG.Proxy @"sSS_anon'anon'x_anon'x") ptr0

instance Marshal.WriteRaw SSS_anon'anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SSS_anon'anon'x sSS_anon'anon'x_anon'x2 ->
            HasCField.writeRaw (BG.Proxy @"sSS_anon'anon'x_anon'x") ptr0 sSS_anon'anon'x_anon'x2

deriving via Marshal.EquivStorable SSS_anon'anon'x instance BG.Storable SSS_anon'anon'x

{-| __C declaration:__ @anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 17:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ SSS_anon'anon'x_anon'x
         ) => BG.CompatHasField.HasField "sSS_anon'anon'x_anon'x" SSS_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          SSS_anon'anon'x {sSS_anon'anon'x_anon'x = y1}
      , BG.getField @"sSS_anon'anon'x_anon'x" x0
      )

instance ( ty ~ SSS_anon'anon'x_anon'x
         ) => BG.HasField "sSS_anon'anon'x_anon'x" (BG.Ptr SSS_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"sSS_anon'anon'x_anon'x")

instance HasCField.HasCField SSS_anon'anon'x "sSS_anon'anon'x_anon'x" where

  type CFieldType SSS_anon'anon'x "sSS_anon'anon'x_anon'x" =
    SSS_anon'anon'x_anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 18:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.HasField "sSS_anon'anon'x_x" SSS_anon'anon'x ty where

  getField =
    \x0 ->
      BG.getField @"sSS_anon'anon'x_anon'x_x" (BG.getField @"sSS_anon'anon'x_anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 18:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "sSS_anon'anon'x_x" SSS_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"sSS_anon'anon'x_anon'x" x0 (\z2 ->
                                                                        BG.CompatHasField.setField @"sSS_anon'anon'x_anon'x_x" z2 y1)
      , BG.getField @"sSS_anon'anon'x_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "sSS_anon'anon'x_x" (BG.Ptr SSS_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"sSS_anon'anon'x_x")

instance HasCField.HasCField SSS_anon'anon'x "sSS_anon'anon'x_x" where

  type CFieldType SSS_anon'anon'x "sSS_anon'anon'x_x" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct SSS@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 15:8@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SSS = SSS
  { sSS_anon'anon'x :: SSS_anon'anon'x
    {- ^ __C declaration:__ @anon\'anon\'x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 16:3@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize SSS where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SSS where

  readRaw =
    \ptr0 ->
          pure SSS
      <*> HasCField.readRaw (BG.Proxy @"sSS_anon'anon'x") ptr0

instance Marshal.WriteRaw SSS where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SSS sSS_anon'anon'x2 ->
            HasCField.writeRaw (BG.Proxy @"sSS_anon'anon'x") ptr0 sSS_anon'anon'x2

deriving via Marshal.EquivStorable SSS instance BG.Storable SSS

{-| __C declaration:__ @anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 16:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ SSS_anon'anon'x
         ) => BG.CompatHasField.HasField "sSS_anon'anon'x" SSS ty where

  hasField =
    \x0 ->
      (\y1 ->
         SSS {sSS_anon'anon'x = y1}, BG.getField @"sSS_anon'anon'x" x0)

instance ( ty ~ SSS_anon'anon'x
         ) => BG.HasField "sSS_anon'anon'x" (BG.Ptr SSS) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"sSS_anon'anon'x")

instance HasCField.HasCField SSS "sSS_anon'anon'x" where

  type CFieldType SSS "sSS_anon'anon'x" =
    SSS_anon'anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 18:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "sSS_x" SSS ty where

  getField =
    \x0 ->
      BG.getField @"sSS_anon'anon'x_x" (BG.getField @"sSS_anon'anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 18:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "sSS_x" SSS ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"sSS_anon'anon'x" x0 (\z2 ->
                                                                 BG.CompatHasField.setField @"sSS_anon'anon'x_x" z2 y1)
      , BG.getField @"sSS_x" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "sSS_x" (BG.Ptr SSS) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"sSS_x")

instance HasCField.HasCField SSS "sSS_x" where

  type CFieldType SSS "sSS_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@USS_anon\'anon\'x_anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 25:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data USS_anon'anon'x_anon'x = USS_anon'anon'x_anon'x
  { uSS_anon'anon'x_anon'x_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 26:11@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize USS_anon'anon'x_anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw USS_anon'anon'x_anon'x where

  readRaw =
    \ptr0 ->
          pure USS_anon'anon'x_anon'x
      <*> HasCField.readRaw (BG.Proxy @"uSS_anon'anon'x_anon'x_x") ptr0

instance Marshal.WriteRaw USS_anon'anon'x_anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          USS_anon'anon'x_anon'x uSS_anon'anon'x_anon'x_x2 ->
            HasCField.writeRaw (BG.Proxy @"uSS_anon'anon'x_anon'x_x") ptr0 uSS_anon'anon'x_anon'x_x2

deriving via Marshal.EquivStorable USS_anon'anon'x_anon'x instance BG.Storable USS_anon'anon'x_anon'x

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 26:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "uSS_anon'anon'x_anon'x_x" USS_anon'anon'x_anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          USS_anon'anon'x_anon'x {uSS_anon'anon'x_anon'x_x = y1}
      , BG.getField @"uSS_anon'anon'x_anon'x_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "uSS_anon'anon'x_anon'x_x" (BG.Ptr USS_anon'anon'x_anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uSS_anon'anon'x_anon'x_x")

instance HasCField.HasCField USS_anon'anon'x_anon'x "uSS_anon'anon'x_anon'x_x" where

  type CFieldType USS_anon'anon'x_anon'x "uSS_anon'anon'x_anon'x_x" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@USS_anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 24:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data USS_anon'anon'x = USS_anon'anon'x
  { uSS_anon'anon'x_anon'x :: USS_anon'anon'x_anon'x
    {- ^ __C declaration:__ @anon\'x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 25:5@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize USS_anon'anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw USS_anon'anon'x where

  readRaw =
    \ptr0 ->
          pure USS_anon'anon'x
      <*> HasCField.readRaw (BG.Proxy @"uSS_anon'anon'x_anon'x") ptr0

instance Marshal.WriteRaw USS_anon'anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          USS_anon'anon'x uSS_anon'anon'x_anon'x2 ->
            HasCField.writeRaw (BG.Proxy @"uSS_anon'anon'x_anon'x") ptr0 uSS_anon'anon'x_anon'x2

deriving via Marshal.EquivStorable USS_anon'anon'x instance BG.Storable USS_anon'anon'x

{-| __C declaration:__ @anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 25:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ USS_anon'anon'x_anon'x
         ) => BG.CompatHasField.HasField "uSS_anon'anon'x_anon'x" USS_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          USS_anon'anon'x {uSS_anon'anon'x_anon'x = y1}
      , BG.getField @"uSS_anon'anon'x_anon'x" x0
      )

instance ( ty ~ USS_anon'anon'x_anon'x
         ) => BG.HasField "uSS_anon'anon'x_anon'x" (BG.Ptr USS_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uSS_anon'anon'x_anon'x")

instance HasCField.HasCField USS_anon'anon'x "uSS_anon'anon'x_anon'x" where

  type CFieldType USS_anon'anon'x "uSS_anon'anon'x_anon'x" =
    USS_anon'anon'x_anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 26:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.HasField "uSS_anon'anon'x_x" USS_anon'anon'x ty where

  getField =
    \x0 ->
      BG.getField @"uSS_anon'anon'x_anon'x_x" (BG.getField @"uSS_anon'anon'x_anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 26:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "uSS_anon'anon'x_x" USS_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"uSS_anon'anon'x_anon'x" x0 (\z2 ->
                                                                        BG.CompatHasField.setField @"uSS_anon'anon'x_anon'x_x" z2 y1)
      , BG.getField @"uSS_anon'anon'x_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "uSS_anon'anon'x_x" (BG.Ptr USS_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uSS_anon'anon'x_x")

instance HasCField.HasCField USS_anon'anon'x "uSS_anon'anon'x_x" where

  type CFieldType USS_anon'anon'x "uSS_anon'anon'x_x" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union USS@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 23:7@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype USS = USS
  { unwrapUSS :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize USS

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw USS

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw USS

deriving via Marshal.EquivStorable USS instance BG.Storable USS

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion USS

{-| __C declaration:__ @anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 24:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance (ty ~ USS_anon'anon'x) => BG.HasField "uSS_anon'anon'x" USS ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 24:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ USS_anon'anon'x
         ) => BG.CompatHasField.HasField "uSS_anon'anon'x" USS ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"uSS_anon'anon'x" x0)

instance ( ty ~ USS_anon'anon'x
         ) => BG.HasField "uSS_anon'anon'x" (BG.Ptr USS) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uSS_anon'anon'x")

instance HasCField.HasCField USS "uSS_anon'anon'x" where

  type CFieldType USS "uSS_anon'anon'x" =
    USS_anon'anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 26:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "uSS_x" USS ty where

  getField =
    \x0 ->
      BG.getField @"uSS_anon'anon'x_x" (BG.getField @"uSS_anon'anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 26:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "uSS_x" USS ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"uSS_anon'anon'x" x0 (\z2 ->
                                                                 BG.CompatHasField.setField @"uSS_anon'anon'x_x" z2 y1)
      , BG.getField @"uSS_x" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "uSS_x" (BG.Ptr USS) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"uSS_x")

instance HasCField.HasCField USS "uSS_x" where

  type CFieldType USS "uSS_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@SUS_anon\'anon\'x_anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 33:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SUS_anon'anon'x_anon'x = SUS_anon'anon'x_anon'x
  { sUS_anon'anon'x_anon'x_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 34:11@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize SUS_anon'anon'x_anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SUS_anon'anon'x_anon'x where

  readRaw =
    \ptr0 ->
          pure SUS_anon'anon'x_anon'x
      <*> HasCField.readRaw (BG.Proxy @"sUS_anon'anon'x_anon'x_x") ptr0

instance Marshal.WriteRaw SUS_anon'anon'x_anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SUS_anon'anon'x_anon'x sUS_anon'anon'x_anon'x_x2 ->
            HasCField.writeRaw (BG.Proxy @"sUS_anon'anon'x_anon'x_x") ptr0 sUS_anon'anon'x_anon'x_x2

deriving via Marshal.EquivStorable SUS_anon'anon'x_anon'x instance BG.Storable SUS_anon'anon'x_anon'x

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 34:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "sUS_anon'anon'x_anon'x_x" SUS_anon'anon'x_anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          SUS_anon'anon'x_anon'x {sUS_anon'anon'x_anon'x_x = y1}
      , BG.getField @"sUS_anon'anon'x_anon'x_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "sUS_anon'anon'x_anon'x_x" (BG.Ptr SUS_anon'anon'x_anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"sUS_anon'anon'x_anon'x_x")

instance HasCField.HasCField SUS_anon'anon'x_anon'x "sUS_anon'anon'x_anon'x_x" where

  type CFieldType SUS_anon'anon'x_anon'x "sUS_anon'anon'x_anon'x_x" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@SUS_anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 32:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype SUS_anon'anon'x = SUS_anon'anon'x
  { unwrapSUS_anon'anon'x :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize SUS_anon'anon'x

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw SUS_anon'anon'x

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw SUS_anon'anon'x

deriving via Marshal.EquivStorable SUS_anon'anon'x instance BG.Storable SUS_anon'anon'x

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion SUS_anon'anon'x

{-| __C declaration:__ @anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 33:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ SUS_anon'anon'x_anon'x
         ) => BG.HasField "sUS_anon'anon'x_anon'x" SUS_anon'anon'x ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 33:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ SUS_anon'anon'x_anon'x
         ) => BG.CompatHasField.HasField "sUS_anon'anon'x_anon'x" SUS_anon'anon'x ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"sUS_anon'anon'x_anon'x" x0)

instance ( ty ~ SUS_anon'anon'x_anon'x
         ) => BG.HasField "sUS_anon'anon'x_anon'x" (BG.Ptr SUS_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"sUS_anon'anon'x_anon'x")

instance HasCField.HasCField SUS_anon'anon'x "sUS_anon'anon'x_anon'x" where

  type CFieldType SUS_anon'anon'x "sUS_anon'anon'x_anon'x" =
    SUS_anon'anon'x_anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 34:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.HasField "sUS_anon'anon'x_x" SUS_anon'anon'x ty where

  getField =
    \x0 ->
      BG.getField @"sUS_anon'anon'x_anon'x_x" (BG.getField @"sUS_anon'anon'x_anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 34:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "sUS_anon'anon'x_x" SUS_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"sUS_anon'anon'x_anon'x" x0 (\z2 ->
                                                                        BG.CompatHasField.setField @"sUS_anon'anon'x_anon'x_x" z2 y1)
      , BG.getField @"sUS_anon'anon'x_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "sUS_anon'anon'x_x" (BG.Ptr SUS_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"sUS_anon'anon'x_x")

instance HasCField.HasCField SUS_anon'anon'x "sUS_anon'anon'x_x" where

  type CFieldType SUS_anon'anon'x "sUS_anon'anon'x_x" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct SUS@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 31:8@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SUS = SUS
  { sUS_anon'anon'x :: SUS_anon'anon'x
    {- ^ __C declaration:__ @anon\'anon\'x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 32:3@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (BG.Generic)

instance Marshal.StaticSize SUS where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SUS where

  readRaw =
    \ptr0 ->
          pure SUS
      <*> HasCField.readRaw (BG.Proxy @"sUS_anon'anon'x") ptr0

instance Marshal.WriteRaw SUS where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SUS sUS_anon'anon'x2 ->
            HasCField.writeRaw (BG.Proxy @"sUS_anon'anon'x") ptr0 sUS_anon'anon'x2

deriving via Marshal.EquivStorable SUS instance BG.Storable SUS

{-| __C declaration:__ @anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 32:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ SUS_anon'anon'x
         ) => BG.CompatHasField.HasField "sUS_anon'anon'x" SUS ty where

  hasField =
    \x0 ->
      (\y1 ->
         SUS {sUS_anon'anon'x = y1}, BG.getField @"sUS_anon'anon'x" x0)

instance ( ty ~ SUS_anon'anon'x
         ) => BG.HasField "sUS_anon'anon'x" (BG.Ptr SUS) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"sUS_anon'anon'x")

instance HasCField.HasCField SUS "sUS_anon'anon'x" where

  type CFieldType SUS "sUS_anon'anon'x" =
    SUS_anon'anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 34:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "sUS_x" SUS ty where

  getField =
    \x0 ->
      BG.getField @"sUS_anon'anon'x_x" (BG.getField @"sUS_anon'anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 34:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "sUS_x" SUS ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"sUS_anon'anon'x" x0 (\z2 ->
                                                                 BG.CompatHasField.setField @"sUS_anon'anon'x_x" z2 y1)
      , BG.getField @"sUS_x" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "sUS_x" (BG.Ptr SUS) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"sUS_x")

instance HasCField.HasCField SUS "sUS_x" where

  type CFieldType SUS "sUS_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@UUS_anon\'anon\'x_anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 41:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data UUS_anon'anon'x_anon'x = UUS_anon'anon'x_anon'x
  { uUS_anon'anon'x_anon'x_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 42:11@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize UUS_anon'anon'x_anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw UUS_anon'anon'x_anon'x where

  readRaw =
    \ptr0 ->
          pure UUS_anon'anon'x_anon'x
      <*> HasCField.readRaw (BG.Proxy @"uUS_anon'anon'x_anon'x_x") ptr0

instance Marshal.WriteRaw UUS_anon'anon'x_anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          UUS_anon'anon'x_anon'x uUS_anon'anon'x_anon'x_x2 ->
            HasCField.writeRaw (BG.Proxy @"uUS_anon'anon'x_anon'x_x") ptr0 uUS_anon'anon'x_anon'x_x2

deriving via Marshal.EquivStorable UUS_anon'anon'x_anon'x instance BG.Storable UUS_anon'anon'x_anon'x

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 42:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "uUS_anon'anon'x_anon'x_x" UUS_anon'anon'x_anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          UUS_anon'anon'x_anon'x {uUS_anon'anon'x_anon'x_x = y1}
      , BG.getField @"uUS_anon'anon'x_anon'x_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "uUS_anon'anon'x_anon'x_x" (BG.Ptr UUS_anon'anon'x_anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uUS_anon'anon'x_anon'x_x")

instance HasCField.HasCField UUS_anon'anon'x_anon'x "uUS_anon'anon'x_anon'x_x" where

  type CFieldType UUS_anon'anon'x_anon'x "uUS_anon'anon'x_anon'x_x" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@UUS_anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 40:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype UUS_anon'anon'x = UUS_anon'anon'x
  { unwrapUUS_anon'anon'x :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize UUS_anon'anon'x

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw UUS_anon'anon'x

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw UUS_anon'anon'x

deriving via Marshal.EquivStorable UUS_anon'anon'x instance BG.Storable UUS_anon'anon'x

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion UUS_anon'anon'x

{-| __C declaration:__ @anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 41:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ UUS_anon'anon'x_anon'x
         ) => BG.HasField "uUS_anon'anon'x_anon'x" UUS_anon'anon'x ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 41:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ UUS_anon'anon'x_anon'x
         ) => BG.CompatHasField.HasField "uUS_anon'anon'x_anon'x" UUS_anon'anon'x ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"uUS_anon'anon'x_anon'x" x0)

instance ( ty ~ UUS_anon'anon'x_anon'x
         ) => BG.HasField "uUS_anon'anon'x_anon'x" (BG.Ptr UUS_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uUS_anon'anon'x_anon'x")

instance HasCField.HasCField UUS_anon'anon'x "uUS_anon'anon'x_anon'x" where

  type CFieldType UUS_anon'anon'x "uUS_anon'anon'x_anon'x" =
    UUS_anon'anon'x_anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 42:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.HasField "uUS_anon'anon'x_x" UUS_anon'anon'x ty where

  getField =
    \x0 ->
      BG.getField @"uUS_anon'anon'x_anon'x_x" (BG.getField @"uUS_anon'anon'x_anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 42:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "uUS_anon'anon'x_x" UUS_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"uUS_anon'anon'x_anon'x" x0 (\z2 ->
                                                                        BG.CompatHasField.setField @"uUS_anon'anon'x_anon'x_x" z2 y1)
      , BG.getField @"uUS_anon'anon'x_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "uUS_anon'anon'x_x" (BG.Ptr UUS_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uUS_anon'anon'x_x")

instance HasCField.HasCField UUS_anon'anon'x "uUS_anon'anon'x_x" where

  type CFieldType UUS_anon'anon'x "uUS_anon'anon'x_x" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union UUS@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 39:7@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype UUS = UUS
  { unwrapUUS :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize UUS

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw UUS

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw UUS

deriving via Marshal.EquivStorable UUS instance BG.Storable UUS

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion UUS

{-| __C declaration:__ @anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 40:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance (ty ~ UUS_anon'anon'x) => BG.HasField "uUS_anon'anon'x" UUS ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 40:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ UUS_anon'anon'x
         ) => BG.CompatHasField.HasField "uUS_anon'anon'x" UUS ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"uUS_anon'anon'x" x0)

instance ( ty ~ UUS_anon'anon'x
         ) => BG.HasField "uUS_anon'anon'x" (BG.Ptr UUS) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uUS_anon'anon'x")

instance HasCField.HasCField UUS "uUS_anon'anon'x" where

  type CFieldType UUS "uUS_anon'anon'x" =
    UUS_anon'anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 42:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "uUS_x" UUS ty where

  getField =
    \x0 ->
      BG.getField @"uUS_anon'anon'x_x" (BG.getField @"uUS_anon'anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 42:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "uUS_x" UUS ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"uUS_anon'anon'x" x0 (\z2 ->
                                                                 BG.CompatHasField.setField @"uUS_anon'anon'x_x" z2 y1)
      , BG.getField @"uUS_x" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "uUS_x" (BG.Ptr UUS) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"uUS_x")

instance HasCField.HasCField UUS "uUS_x" where

  type CFieldType UUS "uUS_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@SSU_anon\'anon\'x_anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 49:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype SSU_anon'anon'x_anon'x = SSU_anon'anon'x_anon'x
  { unwrapSSU_anon'anon'x_anon'x :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize SSU_anon'anon'x_anon'x

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw SSU_anon'anon'x_anon'x

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw SSU_anon'anon'x_anon'x

deriving via Marshal.EquivStorable SSU_anon'anon'x_anon'x instance BG.Storable SSU_anon'anon'x_anon'x

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion SSU_anon'anon'x_anon'x

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 50:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.HasField "sSU_anon'anon'x_anon'x_x" SSU_anon'anon'x_anon'x ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 50:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "sSU_anon'anon'x_anon'x_x" SSU_anon'anon'x_anon'x ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"sSU_anon'anon'x_anon'x_x" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "sSU_anon'anon'x_anon'x_x" (BG.Ptr SSU_anon'anon'x_anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"sSU_anon'anon'x_anon'x_x")

instance HasCField.HasCField SSU_anon'anon'x_anon'x "sSU_anon'anon'x_anon'x_x" where

  type CFieldType SSU_anon'anon'x_anon'x "sSU_anon'anon'x_anon'x_x" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@SSU_anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 48:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SSU_anon'anon'x = SSU_anon'anon'x
  { sSU_anon'anon'x_anon'x :: SSU_anon'anon'x_anon'x
    {- ^ __C declaration:__ @anon\'x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 49:5@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (BG.Generic)

instance Marshal.StaticSize SSU_anon'anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SSU_anon'anon'x where

  readRaw =
    \ptr0 ->
          pure SSU_anon'anon'x
      <*> HasCField.readRaw (BG.Proxy @"sSU_anon'anon'x_anon'x") ptr0

instance Marshal.WriteRaw SSU_anon'anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SSU_anon'anon'x sSU_anon'anon'x_anon'x2 ->
            HasCField.writeRaw (BG.Proxy @"sSU_anon'anon'x_anon'x") ptr0 sSU_anon'anon'x_anon'x2

deriving via Marshal.EquivStorable SSU_anon'anon'x instance BG.Storable SSU_anon'anon'x

{-| __C declaration:__ @anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 49:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ SSU_anon'anon'x_anon'x
         ) => BG.CompatHasField.HasField "sSU_anon'anon'x_anon'x" SSU_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          SSU_anon'anon'x {sSU_anon'anon'x_anon'x = y1}
      , BG.getField @"sSU_anon'anon'x_anon'x" x0
      )

instance ( ty ~ SSU_anon'anon'x_anon'x
         ) => BG.HasField "sSU_anon'anon'x_anon'x" (BG.Ptr SSU_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"sSU_anon'anon'x_anon'x")

instance HasCField.HasCField SSU_anon'anon'x "sSU_anon'anon'x_anon'x" where

  type CFieldType SSU_anon'anon'x "sSU_anon'anon'x_anon'x" =
    SSU_anon'anon'x_anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 50:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.HasField "sSU_anon'anon'x_x" SSU_anon'anon'x ty where

  getField =
    \x0 ->
      BG.getField @"sSU_anon'anon'x_anon'x_x" (BG.getField @"sSU_anon'anon'x_anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 50:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "sSU_anon'anon'x_x" SSU_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"sSU_anon'anon'x_anon'x" x0 (\z2 ->
                                                                        BG.CompatHasField.setField @"sSU_anon'anon'x_anon'x_x" z2 y1)
      , BG.getField @"sSU_anon'anon'x_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "sSU_anon'anon'x_x" (BG.Ptr SSU_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"sSU_anon'anon'x_x")

instance HasCField.HasCField SSU_anon'anon'x "sSU_anon'anon'x_x" where

  type CFieldType SSU_anon'anon'x "sSU_anon'anon'x_x" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct SSU@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 47:8@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SSU = SSU
  { sSU_anon'anon'x :: SSU_anon'anon'x
    {- ^ __C declaration:__ @anon\'anon\'x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 48:3@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (BG.Generic)

instance Marshal.StaticSize SSU where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SSU where

  readRaw =
    \ptr0 ->
          pure SSU
      <*> HasCField.readRaw (BG.Proxy @"sSU_anon'anon'x") ptr0

instance Marshal.WriteRaw SSU where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SSU sSU_anon'anon'x2 ->
            HasCField.writeRaw (BG.Proxy @"sSU_anon'anon'x") ptr0 sSU_anon'anon'x2

deriving via Marshal.EquivStorable SSU instance BG.Storable SSU

{-| __C declaration:__ @anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 48:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ SSU_anon'anon'x
         ) => BG.CompatHasField.HasField "sSU_anon'anon'x" SSU ty where

  hasField =
    \x0 ->
      (\y1 ->
         SSU {sSU_anon'anon'x = y1}, BG.getField @"sSU_anon'anon'x" x0)

instance ( ty ~ SSU_anon'anon'x
         ) => BG.HasField "sSU_anon'anon'x" (BG.Ptr SSU) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"sSU_anon'anon'x")

instance HasCField.HasCField SSU "sSU_anon'anon'x" where

  type CFieldType SSU "sSU_anon'anon'x" =
    SSU_anon'anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 50:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "sSU_x" SSU ty where

  getField =
    \x0 ->
      BG.getField @"sSU_anon'anon'x_x" (BG.getField @"sSU_anon'anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 50:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "sSU_x" SSU ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"sSU_anon'anon'x" x0 (\z2 ->
                                                                 BG.CompatHasField.setField @"sSU_anon'anon'x_x" z2 y1)
      , BG.getField @"sSU_x" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "sSU_x" (BG.Ptr SSU) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"sSU_x")

instance HasCField.HasCField SSU "sSU_x" where

  type CFieldType SSU "sSU_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@USU_anon\'anon\'x_anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 57:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype USU_anon'anon'x_anon'x = USU_anon'anon'x_anon'x
  { unwrapUSU_anon'anon'x_anon'x :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize USU_anon'anon'x_anon'x

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw USU_anon'anon'x_anon'x

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw USU_anon'anon'x_anon'x

deriving via Marshal.EquivStorable USU_anon'anon'x_anon'x instance BG.Storable USU_anon'anon'x_anon'x

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion USU_anon'anon'x_anon'x

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 58:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.HasField "uSU_anon'anon'x_anon'x_x" USU_anon'anon'x_anon'x ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 58:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "uSU_anon'anon'x_anon'x_x" USU_anon'anon'x_anon'x ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"uSU_anon'anon'x_anon'x_x" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "uSU_anon'anon'x_anon'x_x" (BG.Ptr USU_anon'anon'x_anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uSU_anon'anon'x_anon'x_x")

instance HasCField.HasCField USU_anon'anon'x_anon'x "uSU_anon'anon'x_anon'x_x" where

  type CFieldType USU_anon'anon'x_anon'x "uSU_anon'anon'x_anon'x_x" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@USU_anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 56:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data USU_anon'anon'x = USU_anon'anon'x
  { uSU_anon'anon'x_anon'x :: USU_anon'anon'x_anon'x
    {- ^ __C declaration:__ @anon\'x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 57:5@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (BG.Generic)

instance Marshal.StaticSize USU_anon'anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw USU_anon'anon'x where

  readRaw =
    \ptr0 ->
          pure USU_anon'anon'x
      <*> HasCField.readRaw (BG.Proxy @"uSU_anon'anon'x_anon'x") ptr0

instance Marshal.WriteRaw USU_anon'anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          USU_anon'anon'x uSU_anon'anon'x_anon'x2 ->
            HasCField.writeRaw (BG.Proxy @"uSU_anon'anon'x_anon'x") ptr0 uSU_anon'anon'x_anon'x2

deriving via Marshal.EquivStorable USU_anon'anon'x instance BG.Storable USU_anon'anon'x

{-| __C declaration:__ @anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 57:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ USU_anon'anon'x_anon'x
         ) => BG.CompatHasField.HasField "uSU_anon'anon'x_anon'x" USU_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          USU_anon'anon'x {uSU_anon'anon'x_anon'x = y1}
      , BG.getField @"uSU_anon'anon'x_anon'x" x0
      )

instance ( ty ~ USU_anon'anon'x_anon'x
         ) => BG.HasField "uSU_anon'anon'x_anon'x" (BG.Ptr USU_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uSU_anon'anon'x_anon'x")

instance HasCField.HasCField USU_anon'anon'x "uSU_anon'anon'x_anon'x" where

  type CFieldType USU_anon'anon'x "uSU_anon'anon'x_anon'x" =
    USU_anon'anon'x_anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 58:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.HasField "uSU_anon'anon'x_x" USU_anon'anon'x ty where

  getField =
    \x0 ->
      BG.getField @"uSU_anon'anon'x_anon'x_x" (BG.getField @"uSU_anon'anon'x_anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 58:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "uSU_anon'anon'x_x" USU_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"uSU_anon'anon'x_anon'x" x0 (\z2 ->
                                                                        BG.CompatHasField.setField @"uSU_anon'anon'x_anon'x_x" z2 y1)
      , BG.getField @"uSU_anon'anon'x_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "uSU_anon'anon'x_x" (BG.Ptr USU_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uSU_anon'anon'x_x")

instance HasCField.HasCField USU_anon'anon'x "uSU_anon'anon'x_x" where

  type CFieldType USU_anon'anon'x "uSU_anon'anon'x_x" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union USU@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 55:7@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype USU = USU
  { unwrapUSU :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize USU

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw USU

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw USU

deriving via Marshal.EquivStorable USU instance BG.Storable USU

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion USU

{-| __C declaration:__ @anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 56:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance (ty ~ USU_anon'anon'x) => BG.HasField "uSU_anon'anon'x" USU ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 56:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ USU_anon'anon'x
         ) => BG.CompatHasField.HasField "uSU_anon'anon'x" USU ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"uSU_anon'anon'x" x0)

instance ( ty ~ USU_anon'anon'x
         ) => BG.HasField "uSU_anon'anon'x" (BG.Ptr USU) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uSU_anon'anon'x")

instance HasCField.HasCField USU "uSU_anon'anon'x" where

  type CFieldType USU "uSU_anon'anon'x" =
    USU_anon'anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 58:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "uSU_x" USU ty where

  getField =
    \x0 ->
      BG.getField @"uSU_anon'anon'x_x" (BG.getField @"uSU_anon'anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 58:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "uSU_x" USU ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"uSU_anon'anon'x" x0 (\z2 ->
                                                                 BG.CompatHasField.setField @"uSU_anon'anon'x_x" z2 y1)
      , BG.getField @"uSU_x" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "uSU_x" (BG.Ptr USU) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"uSU_x")

instance HasCField.HasCField USU "uSU_x" where

  type CFieldType USU "uSU_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@SUU_anon\'anon\'x_anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 65:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype SUU_anon'anon'x_anon'x = SUU_anon'anon'x_anon'x
  { unwrapSUU_anon'anon'x_anon'x :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize SUU_anon'anon'x_anon'x

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw SUU_anon'anon'x_anon'x

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw SUU_anon'anon'x_anon'x

deriving via Marshal.EquivStorable SUU_anon'anon'x_anon'x instance BG.Storable SUU_anon'anon'x_anon'x

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion SUU_anon'anon'x_anon'x

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 66:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.HasField "sUU_anon'anon'x_anon'x_x" SUU_anon'anon'x_anon'x ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 66:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "sUU_anon'anon'x_anon'x_x" SUU_anon'anon'x_anon'x ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"sUU_anon'anon'x_anon'x_x" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "sUU_anon'anon'x_anon'x_x" (BG.Ptr SUU_anon'anon'x_anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"sUU_anon'anon'x_anon'x_x")

instance HasCField.HasCField SUU_anon'anon'x_anon'x "sUU_anon'anon'x_anon'x_x" where

  type CFieldType SUU_anon'anon'x_anon'x "sUU_anon'anon'x_anon'x_x" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@SUU_anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 64:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype SUU_anon'anon'x = SUU_anon'anon'x
  { unwrapSUU_anon'anon'x :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize SUU_anon'anon'x

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw SUU_anon'anon'x

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw SUU_anon'anon'x

deriving via Marshal.EquivStorable SUU_anon'anon'x instance BG.Storable SUU_anon'anon'x

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion SUU_anon'anon'x

{-| __C declaration:__ @anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 65:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ SUU_anon'anon'x_anon'x
         ) => BG.HasField "sUU_anon'anon'x_anon'x" SUU_anon'anon'x ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 65:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ SUU_anon'anon'x_anon'x
         ) => BG.CompatHasField.HasField "sUU_anon'anon'x_anon'x" SUU_anon'anon'x ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"sUU_anon'anon'x_anon'x" x0)

instance ( ty ~ SUU_anon'anon'x_anon'x
         ) => BG.HasField "sUU_anon'anon'x_anon'x" (BG.Ptr SUU_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"sUU_anon'anon'x_anon'x")

instance HasCField.HasCField SUU_anon'anon'x "sUU_anon'anon'x_anon'x" where

  type CFieldType SUU_anon'anon'x "sUU_anon'anon'x_anon'x" =
    SUU_anon'anon'x_anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 66:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.HasField "sUU_anon'anon'x_x" SUU_anon'anon'x ty where

  getField =
    \x0 ->
      BG.getField @"sUU_anon'anon'x_anon'x_x" (BG.getField @"sUU_anon'anon'x_anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 66:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "sUU_anon'anon'x_x" SUU_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"sUU_anon'anon'x_anon'x" x0 (\z2 ->
                                                                        BG.CompatHasField.setField @"sUU_anon'anon'x_anon'x_x" z2 y1)
      , BG.getField @"sUU_anon'anon'x_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "sUU_anon'anon'x_x" (BG.Ptr SUU_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"sUU_anon'anon'x_x")

instance HasCField.HasCField SUU_anon'anon'x "sUU_anon'anon'x_x" where

  type CFieldType SUU_anon'anon'x "sUU_anon'anon'x_x" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct SUU@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 63:8@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SUU = SUU
  { sUU_anon'anon'x :: SUU_anon'anon'x
    {- ^ __C declaration:__ @anon\'anon\'x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 64:3@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (BG.Generic)

instance Marshal.StaticSize SUU where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SUU where

  readRaw =
    \ptr0 ->
          pure SUU
      <*> HasCField.readRaw (BG.Proxy @"sUU_anon'anon'x") ptr0

instance Marshal.WriteRaw SUU where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SUU sUU_anon'anon'x2 ->
            HasCField.writeRaw (BG.Proxy @"sUU_anon'anon'x") ptr0 sUU_anon'anon'x2

deriving via Marshal.EquivStorable SUU instance BG.Storable SUU

{-| __C declaration:__ @anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 64:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ SUU_anon'anon'x
         ) => BG.CompatHasField.HasField "sUU_anon'anon'x" SUU ty where

  hasField =
    \x0 ->
      (\y1 ->
         SUU {sUU_anon'anon'x = y1}, BG.getField @"sUU_anon'anon'x" x0)

instance ( ty ~ SUU_anon'anon'x
         ) => BG.HasField "sUU_anon'anon'x" (BG.Ptr SUU) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"sUU_anon'anon'x")

instance HasCField.HasCField SUU "sUU_anon'anon'x" where

  type CFieldType SUU "sUU_anon'anon'x" =
    SUU_anon'anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 66:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "sUU_x" SUU ty where

  getField =
    \x0 ->
      BG.getField @"sUU_anon'anon'x_x" (BG.getField @"sUU_anon'anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 66:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "sUU_x" SUU ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"sUU_anon'anon'x" x0 (\z2 ->
                                                                 BG.CompatHasField.setField @"sUU_anon'anon'x_x" z2 y1)
      , BG.getField @"sUU_x" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "sUU_x" (BG.Ptr SUU) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"sUU_x")

instance HasCField.HasCField SUU "sUU_x" where

  type CFieldType SUU "sUU_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@UUU_anon\'anon\'x_anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 73:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype UUU_anon'anon'x_anon'x = UUU_anon'anon'x_anon'x
  { unwrapUUU_anon'anon'x_anon'x :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize UUU_anon'anon'x_anon'x

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw UUU_anon'anon'x_anon'x

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw UUU_anon'anon'x_anon'x

deriving via Marshal.EquivStorable UUU_anon'anon'x_anon'x instance BG.Storable UUU_anon'anon'x_anon'x

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion UUU_anon'anon'x_anon'x

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 74:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.HasField "uUU_anon'anon'x_anon'x_x" UUU_anon'anon'x_anon'x ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 74:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "uUU_anon'anon'x_anon'x_x" UUU_anon'anon'x_anon'x ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"uUU_anon'anon'x_anon'x_x" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "uUU_anon'anon'x_anon'x_x" (BG.Ptr UUU_anon'anon'x_anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uUU_anon'anon'x_anon'x_x")

instance HasCField.HasCField UUU_anon'anon'x_anon'x "uUU_anon'anon'x_anon'x_x" where

  type CFieldType UUU_anon'anon'x_anon'x "uUU_anon'anon'x_anon'x_x" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@UUU_anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 72:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype UUU_anon'anon'x = UUU_anon'anon'x
  { unwrapUUU_anon'anon'x :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize UUU_anon'anon'x

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw UUU_anon'anon'x

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw UUU_anon'anon'x

deriving via Marshal.EquivStorable UUU_anon'anon'x instance BG.Storable UUU_anon'anon'x

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion UUU_anon'anon'x

{-| __C declaration:__ @anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 73:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ UUU_anon'anon'x_anon'x
         ) => BG.HasField "uUU_anon'anon'x_anon'x" UUU_anon'anon'x ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 73:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ UUU_anon'anon'x_anon'x
         ) => BG.CompatHasField.HasField "uUU_anon'anon'x_anon'x" UUU_anon'anon'x ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"uUU_anon'anon'x_anon'x" x0)

instance ( ty ~ UUU_anon'anon'x_anon'x
         ) => BG.HasField "uUU_anon'anon'x_anon'x" (BG.Ptr UUU_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uUU_anon'anon'x_anon'x")

instance HasCField.HasCField UUU_anon'anon'x "uUU_anon'anon'x_anon'x" where

  type CFieldType UUU_anon'anon'x "uUU_anon'anon'x_anon'x" =
    UUU_anon'anon'x_anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 74:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.HasField "uUU_anon'anon'x_x" UUU_anon'anon'x ty where

  getField =
    \x0 ->
      BG.getField @"uUU_anon'anon'x_anon'x_x" (BG.getField @"uUU_anon'anon'x_anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 74:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "uUU_anon'anon'x_x" UUU_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"uUU_anon'anon'x_anon'x" x0 (\z2 ->
                                                                        BG.CompatHasField.setField @"uUU_anon'anon'x_anon'x_x" z2 y1)
      , BG.getField @"uUU_anon'anon'x_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "uUU_anon'anon'x_x" (BG.Ptr UUU_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uUU_anon'anon'x_x")

instance HasCField.HasCField UUU_anon'anon'x "uUU_anon'anon'x_x" where

  type CFieldType UUU_anon'anon'x "uUU_anon'anon'x_x" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union UUU@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 71:7@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype UUU = UUU
  { unwrapUUU :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize UUU

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw UUU

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw UUU

deriving via Marshal.EquivStorable UUU instance BG.Storable UUU

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion UUU

{-| __C declaration:__ @anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 72:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance (ty ~ UUU_anon'anon'x) => BG.HasField "uUU_anon'anon'x" UUU ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 72:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance ( ty ~ UUU_anon'anon'x
         ) => BG.CompatHasField.HasField "uUU_anon'anon'x" UUU ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"uUU_anon'anon'x" x0)

instance ( ty ~ UUU_anon'anon'x
         ) => BG.HasField "uUU_anon'anon'x" (BG.Ptr UUU) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uUU_anon'anon'x")

instance HasCField.HasCField UUU "uUU_anon'anon'x" where

  type CFieldType UUU "uUU_anon'anon'x" =
    UUU_anon'anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 74:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "uUU_x" UUU ty where

  getField =
    \x0 ->
      BG.getField @"uUU_anon'anon'x_x" (BG.getField @"uUU_anon'anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 74:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "uUU_x" UUU ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"uUU_anon'anon'x" x0 (\z2 ->
                                                                 BG.CompatHasField.setField @"uUU_anon'anon'x_x" z2 y1)
      , BG.getField @"uUU_x" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "uUU_x" (BG.Ptr UUU) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"uUU_x")

instance HasCField.HasCField UUU "uUU_x" where

  type CFieldType UUU "uUU_x" = BG.CInt

  offset# = \_ -> \_ -> 0
