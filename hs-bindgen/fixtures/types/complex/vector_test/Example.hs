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

{-| __C declaration:__ @struct vector@

    __defined at:__ @types\/complex\/vector_test.h 1:9@

    __exported by:__ @types\/complex\/vector_test.h@
-}
data Vector = Vector
  { vector_x :: RIP.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/complex\/vector_test.h 2:12@

         __exported by:__ @types\/complex\/vector_test.h@
    -}
  , vector_y :: RIP.CDouble
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/complex\/vector_test.h 3:12@

         __exported by:__ @types\/complex\/vector_test.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Vector where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Vector where

  readRaw =
    \ptr0 ->
          pure Vector
      <*> HasCField.readRaw (RIP.Proxy @"vector_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"vector_y") ptr0

instance Marshal.WriteRaw Vector where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Vector vector_x2 vector_y3 ->
               HasCField.writeRaw (RIP.Proxy @"vector_x") ptr0 vector_x2
            >> HasCField.writeRaw (RIP.Proxy @"vector_y") ptr0 vector_y3

deriving via Marshal.EquivStorable Vector instance RIP.Storable Vector

instance HasCField.HasCField Vector "vector_x" where

  type CFieldType Vector "vector_x" = RIP.CDouble

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CDouble
         ) => RIP.HasField "vector_x" (RIP.Ptr Vector) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"vector_x")

instance HasCField.HasCField Vector "vector_y" where

  type CFieldType Vector "vector_y" = RIP.CDouble

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CDouble
         ) => RIP.HasField "vector_y" (RIP.Ptr Vector) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"vector_y")
