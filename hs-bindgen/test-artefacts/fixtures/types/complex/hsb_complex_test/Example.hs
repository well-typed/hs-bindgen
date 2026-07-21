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
    ( Example.Complex_object_t(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct complex_object_t@

    __defined at:__ @types\/complex\/hsb_complex_test.h 24:9@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
data Complex_object_t = Complex_object_t
  { complex_object_t_velocity :: BG.Complex BG.CFloat
    {- ^ __C declaration:__ @velocity@

         __defined at:__ @types\/complex\/hsb_complex_test.h 25:20@

         __exported by:__ @types\/complex\/hsb_complex_test.h@
    -}
  , complex_object_t_position :: BG.Complex BG.CDouble
    {- ^ __C declaration:__ @position@

         __defined at:__ @types\/complex\/hsb_complex_test.h 26:20@

         __exported by:__ @types\/complex\/hsb_complex_test.h@
    -}
  , complex_object_t_id :: BG.CInt
    {- ^ __C declaration:__ @id@

         __defined at:__ @types\/complex\/hsb_complex_test.h 27:9@

         __exported by:__ @types\/complex\/hsb_complex_test.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Complex_object_t where

  staticSizeOf = \_ -> (32 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Complex_object_t where

  readRaw =
    \ptr0 ->
          pure Complex_object_t
      <*> HasCField.readRaw (BG.Proxy @"complex_object_t_velocity") ptr0
      <*> HasCField.readRaw (BG.Proxy @"complex_object_t_position") ptr0
      <*> HasCField.readRaw (BG.Proxy @"complex_object_t_id") ptr0

instance Marshal.WriteRaw Complex_object_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Complex_object_t
            complex_object_t_velocity2
            complex_object_t_position3
            complex_object_t_id4 ->
                 HasCField.writeRaw (BG.Proxy @"complex_object_t_velocity") ptr0 complex_object_t_velocity2
              >> HasCField.writeRaw (BG.Proxy @"complex_object_t_position") ptr0 complex_object_t_position3
              >> HasCField.writeRaw (BG.Proxy @"complex_object_t_id") ptr0 complex_object_t_id4

deriving via Marshal.EquivStorable Complex_object_t instance BG.Storable Complex_object_t

{-| __C declaration:__ @velocity@

    __defined at:__ @types\/complex\/hsb_complex_test.h 25:20@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
instance ( ty ~ BG.Complex BG.CFloat
         ) => BG.CompatHasField.HasField "complex_object_t_velocity" Complex_object_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Complex_object_t { complex_object_t_velocity = y1
                           , complex_object_t_position = BG.getField @"complex_object_t_position" x0
                           , complex_object_t_id = BG.getField @"complex_object_t_id" x0
                           }
      , BG.getField @"complex_object_t_velocity" x0
      )

instance ( ty ~ BG.Complex BG.CFloat
         ) => BG.HasField "complex_object_t_velocity" (BG.Ptr Complex_object_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"complex_object_t_velocity")

instance HasCField.HasCField Complex_object_t "complex_object_t_velocity" where

  type CFieldType Complex_object_t "complex_object_t_velocity" =
    BG.Complex BG.CFloat

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @position@

    __defined at:__ @types\/complex\/hsb_complex_test.h 26:20@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
instance ( ty ~ BG.Complex BG.CDouble
         ) => BG.CompatHasField.HasField "complex_object_t_position" Complex_object_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Complex_object_t { complex_object_t_position = y1
                           , complex_object_t_velocity = BG.getField @"complex_object_t_velocity" x0
                           , complex_object_t_id = BG.getField @"complex_object_t_id" x0
                           }
      , BG.getField @"complex_object_t_position" x0
      )

instance ( ty ~ BG.Complex BG.CDouble
         ) => BG.HasField "complex_object_t_position" (BG.Ptr Complex_object_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"complex_object_t_position")

instance HasCField.HasCField Complex_object_t "complex_object_t_position" where

  type CFieldType Complex_object_t "complex_object_t_position" =
    BG.Complex BG.CDouble

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @id@

    __defined at:__ @types\/complex\/hsb_complex_test.h 27:9@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "complex_object_t_id" Complex_object_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Complex_object_t { complex_object_t_id = y1
                           , complex_object_t_velocity = BG.getField @"complex_object_t_velocity" x0
                           , complex_object_t_position = BG.getField @"complex_object_t_position" x0
                           }
      , BG.getField @"complex_object_t_id" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "complex_object_t_id" (BG.Ptr Complex_object_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"complex_object_t_id")

instance HasCField.HasCField Complex_object_t "complex_object_t_id" where

  type CFieldType Complex_object_t "complex_object_t_id" =
    BG.CInt

  offset# = \_ -> \_ -> 24
