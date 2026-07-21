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
    ( Example.Struct2(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct struct2@

    __defined at:__ @types\/special\/parse_failure_long_double.h 13:8@

    __exported by:__ @types\/special\/parse_failure_long_double.h@
-}
data Struct2 = Struct2
  { struct2_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/special\/parse_failure_long_double.h 14:7@

         __exported by:__ @types\/special\/parse_failure_long_double.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Struct2 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Struct2 where

  readRaw =
    \ptr0 ->
          pure Struct2
      <*> HasCField.readRaw (BG.Proxy @"struct2_x") ptr0

instance Marshal.WriteRaw Struct2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2 struct2_x2 ->
            HasCField.writeRaw (BG.Proxy @"struct2_x") ptr0 struct2_x2

deriving via Marshal.EquivStorable Struct2 instance BG.Storable Struct2

{-| __C declaration:__ @x@

    __defined at:__ @types\/special\/parse_failure_long_double.h 14:7@

    __exported by:__ @types\/special\/parse_failure_long_double.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "struct2_x" Struct2 ty where

  hasField =
    \x0 ->
      (\y1 ->
         Struct2 {struct2_x = y1}, BG.getField @"struct2_x" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "struct2_x" (BG.Ptr Struct2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"struct2_x")

instance HasCField.HasCField Struct2 "struct2_x" where

  type CFieldType Struct2 "struct2_x" = BG.CInt

  offset# = \_ -> \_ -> 0
