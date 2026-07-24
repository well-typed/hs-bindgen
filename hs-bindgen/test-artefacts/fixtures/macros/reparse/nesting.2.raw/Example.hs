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
    ( Example.myInt
    , Example.T2(..)
    , Example.T3(..)
    , Example.T4(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @macro MyInt@

    __defined at:__ @macros\/reparse\/nesting.h 1:9@

    __exported by:__ @macros\/reparse\/nesting.h@
-}
myInt :: [String]
myInt = ["int"]

{-| __C declaration:__ @struct TS1@

    __defined at:__ @macros\/reparse\/nesting.h 5:16@

    __exported by:__ @macros\/reparse\/nesting.h@
-}
data T2 = T2
  { t2_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting.h 5:28@

         __exported by:__ @macros\/reparse\/nesting.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize T2 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T2 where

  readRaw =
    \ptr0 ->
          pure T2
      <*> HasCField.readRaw (BG.Proxy @"t2_x") ptr0

instance Marshal.WriteRaw T2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T2 t2_x2 ->
            HasCField.writeRaw (BG.Proxy @"t2_x") ptr0 t2_x2

deriving via Marshal.EquivStorable T2 instance BG.Storable T2

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting.h 5:28@

    __exported by:__ @macros\/reparse\/nesting.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "t2_x" T2 ty where

  hasField =
    \x0 ->
      (\y1 -> T2 {t2_x = y1}, BG.getField @"t2_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "t2_x" (BG.Ptr T2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t2_x")

instance HasCField.HasCField T2 "t2_x" where

  type CFieldType T2 "t2_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct TS3@

    __defined at:__ @macros\/reparse\/nesting.h 6:16@

    __exported by:__ @macros\/reparse\/nesting.h@
-}
data T3 = T3
  { t3_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting.h 6:28@

         __exported by:__ @macros\/reparse\/nesting.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize T3 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T3 where

  readRaw =
    \ptr0 ->
          pure T3
      <*> HasCField.readRaw (BG.Proxy @"t3_x") ptr0

instance Marshal.WriteRaw T3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T3 t3_x2 ->
            HasCField.writeRaw (BG.Proxy @"t3_x") ptr0 t3_x2

deriving via Marshal.EquivStorable T3 instance BG.Storable T3

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting.h 6:28@

    __exported by:__ @macros\/reparse\/nesting.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "t3_x" T3 ty where

  hasField =
    \x0 ->
      (\y1 -> T3 {t3_x = y1}, BG.getField @"t3_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "t3_x" (BG.Ptr T3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t3_x")

instance HasCField.HasCField T3 "t3_x" where

  type CFieldType T3 "t3_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct T4@

    __defined at:__ @macros\/reparse\/nesting.h 7:9@

    __exported by:__ @macros\/reparse\/nesting.h@
-}
data T4 = T4
  { t4_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting.h 7:28@

         __exported by:__ @macros\/reparse\/nesting.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize T4 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T4 where

  readRaw =
    \ptr0 ->
          pure T4
      <*> HasCField.readRaw (BG.Proxy @"t4_x") ptr0

instance Marshal.WriteRaw T4 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T4 t4_x2 ->
            HasCField.writeRaw (BG.Proxy @"t4_x") ptr0 t4_x2

deriving via Marshal.EquivStorable T4 instance BG.Storable T4

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting.h 7:28@

    __exported by:__ @macros\/reparse\/nesting.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "t4_x" T4 ty where

  hasField =
    \x0 ->
      (\y1 -> T4 {t4_x = y1}, BG.getField @"t4_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "t4_x" (BG.Ptr T4) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t4_x")

instance HasCField.HasCField T4 "t4_x" where

  type CFieldType T4 "t4_x" = BG.CInt

  offset# = \_ -> \_ -> 0
