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
    ( Example.Banner_point(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| A struct documented with a Javadoc banner comment.

    __C declaration:__ @struct banner_point@

    __defined at:__ @documentation\/javadoc_banner.h 19:8@

    __exported by:__ @documentation\/javadoc_banner.h@
-}
data Banner_point = Banner_point
  { banner_point_x :: BG.CInt
    {- ^ X coordinate

         __C declaration:__ @x@

         __defined at:__ @documentation\/javadoc_banner.h 23:9@

         __exported by:__ @documentation\/javadoc_banner.h@
    -}
  , banner_point_y :: BG.CInt
    {- ^ Y coordinate

         __C declaration:__ @y@

         __defined at:__ @documentation\/javadoc_banner.h 28:9@

         __exported by:__ @documentation\/javadoc_banner.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Banner_point where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Banner_point where

  readRaw =
    \ptr0 ->
          pure Banner_point
      <*> HasCField.readRaw (BG.Proxy @"banner_point_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"banner_point_y") ptr0

instance Marshal.WriteRaw Banner_point where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Banner_point banner_point_x2 banner_point_y3 ->
               HasCField.writeRaw (BG.Proxy @"banner_point_x") ptr0 banner_point_x2
            >> HasCField.writeRaw (BG.Proxy @"banner_point_y") ptr0 banner_point_y3

deriving via Marshal.EquivStorable Banner_point instance BG.Storable Banner_point

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "banner_point_x" Banner_point ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Banner_point {banner_point_x = y1, banner_point_y = BG.getField @"banner_point_y" x0}
      , BG.getField @"banner_point_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "banner_point_x" (BG.Ptr Banner_point) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"banner_point_x")

instance HasCField.HasCField Banner_point "banner_point_x" where

  type CFieldType Banner_point "banner_point_x" =
    BG.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "banner_point_y" Banner_point ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Banner_point {banner_point_y = y1, banner_point_x = BG.getField @"banner_point_x" x0}
      , BG.getField @"banner_point_y" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "banner_point_y" (BG.Ptr Banner_point) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"banner_point_y")

instance HasCField.HasCField Banner_point "banner_point_y" where

  type CFieldType Banner_point "banner_point_y" =
    BG.CInt

  offset# = \_ -> \_ -> 4
