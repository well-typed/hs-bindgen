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
    ( Example.Pt(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct pt@

    __defined at:__ @binding-specs\/enum\/mismatch.h 1:8@

    __exported by:__ @binding-specs\/enum\/mismatch.h@
-}
data Pt = Pt
  { pt_x :: BG.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @binding-specs\/enum\/mismatch.h 1:20@

         __exported by:__ @binding-specs\/enum\/mismatch.h@
    -}
  , pt_y :: BG.CDouble
    {- ^ __C declaration:__ @y@

         __defined at:__ @binding-specs\/enum\/mismatch.h 1:23@

         __exported by:__ @binding-specs\/enum\/mismatch.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Pt where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Pt where

  readRaw =
    \ptr0 ->
          pure Pt
      <*> HasCField.readRaw (BG.Proxy @"pt_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"pt_y") ptr0

instance Marshal.WriteRaw Pt where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Pt pt_x2 pt_y3 ->
               HasCField.writeRaw (BG.Proxy @"pt_x") ptr0 pt_x2
            >> HasCField.writeRaw (BG.Proxy @"pt_y") ptr0 pt_y3

deriving via Marshal.EquivStorable Pt instance BG.Storable Pt

{-| __C declaration:__ @x@

    __defined at:__ @binding-specs\/enum\/mismatch.h 1:20@

    __exported by:__ @binding-specs\/enum\/mismatch.h@
-}
instance (ty ~ BG.CDouble) => BG.CompatHasField.HasField "pt_x" Pt ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Pt {pt_x = y1, pt_y = BG.getField @"pt_y" x0}
      , BG.getField @"pt_x" x0
      )

instance (ty ~ BG.CDouble) => BG.HasField "pt_x" (BG.Ptr Pt) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"pt_x")

instance HasCField.HasCField Pt "pt_x" where

  type CFieldType Pt "pt_x" = BG.CDouble

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @binding-specs\/enum\/mismatch.h 1:23@

    __exported by:__ @binding-specs\/enum\/mismatch.h@
-}
instance (ty ~ BG.CDouble) => BG.CompatHasField.HasField "pt_y" Pt ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Pt {pt_y = y1, pt_x = BG.getField @"pt_x" x0}
      , BG.getField @"pt_y" x0
      )

instance (ty ~ BG.CDouble) => BG.HasField "pt_y" (BG.Ptr Pt) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"pt_y")

instance HasCField.HasCField Pt "pt_y" where

  type CFieldType Pt "pt_y" = BG.CDouble

  offset# = \_ -> \_ -> 8
