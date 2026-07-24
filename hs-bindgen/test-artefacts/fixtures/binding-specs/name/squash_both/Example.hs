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
    ( Example.Hoge(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct foo@

    __defined at:__ @binding-specs\/name\/squash_both.h 1:16@

    __exported by:__ @binding-specs\/name\/squash_both.h@
-}
data Hoge = Hoge
  { hoge_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @binding-specs\/name\/squash_both.h 1:26@

         __exported by:__ @binding-specs\/name\/squash_both.h@
    -}
  , hoge_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @binding-specs\/name\/squash_both.h 1:29@

         __exported by:__ @binding-specs\/name\/squash_both.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Hoge where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Hoge where

  readRaw =
    \ptr0 ->
          pure Hoge
      <*> HasCField.readRaw (BG.Proxy @"hoge_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"hoge_y") ptr0

instance Marshal.WriteRaw Hoge where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Hoge hoge_x2 hoge_y3 ->
               HasCField.writeRaw (BG.Proxy @"hoge_x") ptr0 hoge_x2
            >> HasCField.writeRaw (BG.Proxy @"hoge_y") ptr0 hoge_y3

deriving via Marshal.EquivStorable Hoge instance BG.Storable Hoge

deriving via Struct.IsStructViaStorable Hoge instance Struct.IsStruct Hoge

{-| __C declaration:__ @x@

    __defined at:__ @binding-specs\/name\/squash_both.h 1:26@

    __exported by:__ @binding-specs\/name\/squash_both.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "hoge_x" Hoge ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Hoge {hoge_x = y1, hoge_y = BG.getField @"hoge_y" x0}
      , BG.getField @"hoge_x" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "hoge_x" (BG.Ptr Hoge) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"hoge_x")

instance HasCField.HasCField Hoge "hoge_x" where

  type CFieldType Hoge "hoge_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @binding-specs\/name\/squash_both.h 1:29@

    __exported by:__ @binding-specs\/name\/squash_both.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "hoge_y" Hoge ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Hoge {hoge_y = y1, hoge_x = BG.getField @"hoge_x" x0}
      , BG.getField @"hoge_y" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "hoge_y" (BG.Ptr Hoge) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"hoge_y")

instance HasCField.HasCField Hoge "hoge_y" where

  type CFieldType Hoge "hoge_y" = BG.CInt

  offset# = \_ -> \_ -> 4
