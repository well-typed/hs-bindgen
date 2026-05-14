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
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| A struct documented with a Javadoc banner comment.

    __C declaration:__ @struct banner_point@

    __defined at:__ @documentation\/javadoc_banner.h 19:8@

    __exported by:__ @documentation\/javadoc_banner.h@
-}
data Banner_point = Banner_point
  { banner_point_x :: RIP.CInt
    {- ^ X coordinate

         __C declaration:__ @x@

         __defined at:__ @documentation\/javadoc_banner.h 23:9@

         __exported by:__ @documentation\/javadoc_banner.h@
    -}
  , banner_point_y :: RIP.CInt
    {- ^ Y coordinate

         __C declaration:__ @y@

         __defined at:__ @documentation\/javadoc_banner.h 28:9@

         __exported by:__ @documentation\/javadoc_banner.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Banner_point where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Banner_point where

  readRaw =
    \ptr0 ->
          pure Banner_point
      <*> HasCField.readRaw (RIP.Proxy @"banner_point_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"banner_point_y") ptr0

instance Marshal.WriteRaw Banner_point where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Banner_point banner_point_x2 banner_point_y3 ->
               HasCField.writeRaw (RIP.Proxy @"banner_point_x") ptr0 banner_point_x2
            >> HasCField.writeRaw (RIP.Proxy @"banner_point_y") ptr0 banner_point_y3

deriving via Marshal.EquivStorable Banner_point instance RIP.Storable Banner_point

instance HasCField.HasCField Banner_point "banner_point_x" where

  type CFieldType Banner_point "banner_point_x" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "banner_point_x" (RIP.Ptr Banner_point) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"banner_point_x")

instance HasCField.HasCField Banner_point "banner_point_y" where

  type CFieldType Banner_point "banner_point_y" =
    RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "banner_point_y" (RIP.Ptr Banner_point) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"banner_point_y")
