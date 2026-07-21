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
    ( Example.Vector(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct vector@

    __defined at:__ @types\/complex\/vector_test.h 1:9@

    __exported by:__ @types\/complex\/vector_test.h@
-}
data Vector = Vector
  { vector_x :: BG.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/complex\/vector_test.h 2:12@

         __exported by:__ @types\/complex\/vector_test.h@
    -}
  , vector_y :: BG.CDouble
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/complex\/vector_test.h 3:12@

         __exported by:__ @types\/complex\/vector_test.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Vector where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Vector where

  readRaw =
    \ptr0 ->
          pure Vector
      <*> HasCField.readRaw (BG.Proxy @"vector_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"vector_y") ptr0

instance Marshal.WriteRaw Vector where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Vector vector_x2 vector_y3 ->
               HasCField.writeRaw (BG.Proxy @"vector_x") ptr0 vector_x2
            >> HasCField.writeRaw (BG.Proxy @"vector_y") ptr0 vector_y3

deriving via Marshal.EquivStorable Vector instance BG.Storable Vector

{-| __C declaration:__ @x@

    __defined at:__ @types\/complex\/vector_test.h 2:12@

    __exported by:__ @types\/complex\/vector_test.h@
-}
instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "vector_x" Vector ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Vector {vector_x = y1, vector_y = BG.getField @"vector_y" x0}
      , BG.getField @"vector_x" x0
      )

instance ( ty ~ BG.CDouble
         ) => BG.HasField "vector_x" (BG.Ptr Vector) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"vector_x")

instance HasCField.HasCField Vector "vector_x" where

  type CFieldType Vector "vector_x" = BG.CDouble

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @types\/complex\/vector_test.h 3:12@

    __exported by:__ @types\/complex\/vector_test.h@
-}
instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "vector_y" Vector ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Vector {vector_y = y1, vector_x = BG.getField @"vector_x" x0}
      , BG.getField @"vector_y" x0
      )

instance ( ty ~ BG.CDouble
         ) => BG.HasField "vector_y" (BG.Ptr Vector) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"vector_y")

instance HasCField.HasCField Vector "vector_y" where

  type CFieldType Vector "vector_y" = BG.CDouble

  offset# = \_ -> \_ -> 8
