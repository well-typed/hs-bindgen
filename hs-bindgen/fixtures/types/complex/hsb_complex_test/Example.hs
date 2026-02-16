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

{-| __C declaration:__ @struct complex_object_t@

    __defined at:__ @types\/complex\/hsb_complex_test.h 24:9@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
data Complex_object_t = Complex_object_t
  { complex_object_t_velocity :: RIP.Complex RIP.CFloat
    {- ^ __C declaration:__ @velocity@

         __defined at:__ @types\/complex\/hsb_complex_test.h 25:20@

         __exported by:__ @types\/complex\/hsb_complex_test.h@
    -}
  , complex_object_t_position :: RIP.Complex RIP.CDouble
    {- ^ __C declaration:__ @position@

         __defined at:__ @types\/complex\/hsb_complex_test.h 26:20@

         __exported by:__ @types\/complex\/hsb_complex_test.h@
    -}
  , complex_object_t_id :: RIP.CInt
    {- ^ __C declaration:__ @id@

         __defined at:__ @types\/complex\/hsb_complex_test.h 27:9@

         __exported by:__ @types\/complex\/hsb_complex_test.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Complex_object_t where

  staticSizeOf = \_ -> (32 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Complex_object_t where

  readRaw =
    \ptr0 ->
          pure Complex_object_t
      <*> HasCField.readRaw (RIP.Proxy @"complex_object_t_velocity") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"complex_object_t_position") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"complex_object_t_id") ptr0

instance Marshal.WriteRaw Complex_object_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Complex_object_t
            complex_object_t_velocity2
            complex_object_t_position3
            complex_object_t_id4 ->
                 HasCField.writeRaw (RIP.Proxy @"complex_object_t_velocity") ptr0 complex_object_t_velocity2
              >> HasCField.writeRaw (RIP.Proxy @"complex_object_t_position") ptr0 complex_object_t_position3
              >> HasCField.writeRaw (RIP.Proxy @"complex_object_t_id") ptr0 complex_object_t_id4

deriving via Marshal.EquivStorable Complex_object_t instance RIP.Storable Complex_object_t

instance HasCField.HasCField Complex_object_t "complex_object_t_velocity" where

  type CFieldType Complex_object_t "complex_object_t_velocity" =
    RIP.Complex RIP.CFloat

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) (RIP.Complex RIP.CFloat)
         ) => RIP.HasField "complex_object_t_velocity" (RIP.Ptr Complex_object_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"complex_object_t_velocity")

instance HasCField.HasCField Complex_object_t "complex_object_t_position" where

  type CFieldType Complex_object_t "complex_object_t_position" =
    RIP.Complex RIP.CDouble

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) (RIP.Complex RIP.CDouble)
         ) => RIP.HasField "complex_object_t_position" (RIP.Ptr Complex_object_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"complex_object_t_position")

instance HasCField.HasCField Complex_object_t "complex_object_t_id" where

  type CFieldType Complex_object_t "complex_object_t_id" =
    RIP.CInt

  offset# = \_ -> \_ -> 24

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "complex_object_t_id" (RIP.Ptr Complex_object_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"complex_object_t_id")
