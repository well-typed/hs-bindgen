{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct struct2@

    __defined at:__ @types\/special\/parse_failure_long_double.h 13:8@

    __exported by:__ @types\/special\/parse_failure_long_double.h@
-}
data Struct2 = Struct2
  { struct2_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/special\/parse_failure_long_double.h 14:7@

         __exported by:__ @types\/special\/parse_failure_long_double.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct2 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Struct2 where

  readRaw =
    \ptr0 ->
          pure Struct2
      <*> HasCField.readRaw (RIP.Proxy @"struct2_x") ptr0

instance Marshal.WriteRaw Struct2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2 struct2_x2 ->
            HasCField.writeRaw (RIP.Proxy @"struct2_x") ptr0 struct2_x2

deriving via Marshal.EquivStorable Struct2 instance RIP.Storable Struct2

instance HasCField.HasCField Struct2 "struct2_x" where

  type CFieldType Struct2 "struct2_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "struct2_x" (RIP.Ptr Struct2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"struct2_x")
