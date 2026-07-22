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
    ( Example.Piyo(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct foo@

    __defined at:__ @binding-specs\/name\/squash_typedef.h 1:16@

    __exported by:__ @binding-specs\/name\/squash_typedef.h@
-}
data Piyo = Piyo
  { piyo_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @binding-specs\/name\/squash_typedef.h 1:26@

         __exported by:__ @binding-specs\/name\/squash_typedef.h@
    -}
  , piyo_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @binding-specs\/name\/squash_typedef.h 1:29@

         __exported by:__ @binding-specs\/name\/squash_typedef.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Piyo where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Piyo where

  readRaw =
    \ptr0 ->
          pure Piyo
      <*> HasCField.readRaw (BG.Proxy @"piyo_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"piyo_y") ptr0

instance Marshal.WriteRaw Piyo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Piyo piyo_x2 piyo_y3 ->
               HasCField.writeRaw (BG.Proxy @"piyo_x") ptr0 piyo_x2
            >> HasCField.writeRaw (BG.Proxy @"piyo_y") ptr0 piyo_y3

deriving via Marshal.EquivStorable Piyo instance BG.Storable Piyo

{-| __C declaration:__ @x@

    __defined at:__ @binding-specs\/name\/squash_typedef.h 1:26@

    __exported by:__ @binding-specs\/name\/squash_typedef.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "piyo_x" Piyo ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Piyo {piyo_x = y1, piyo_y = BG.getField @"piyo_y" x0}
      , BG.getField @"piyo_x" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "piyo_x" (BG.Ptr Piyo) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"piyo_x")

instance HasCField.HasCField Piyo "piyo_x" where

  type CFieldType Piyo "piyo_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @binding-specs\/name\/squash_typedef.h 1:29@

    __exported by:__ @binding-specs\/name\/squash_typedef.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "piyo_y" Piyo ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Piyo {piyo_y = y1, piyo_x = BG.getField @"piyo_x" x0}
      , BG.getField @"piyo_y" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "piyo_y" (BG.Ptr Piyo) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"piyo_y")

instance HasCField.HasCField Piyo "piyo_y" where

  type CFieldType Piyo "piyo_y" = BG.CInt

  offset# = \_ -> \_ -> 4
