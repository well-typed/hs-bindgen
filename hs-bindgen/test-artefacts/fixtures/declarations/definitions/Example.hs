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
    ( Example.X(..)
    , Example.Y(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @struct X@

    __defined at:__ @declarations\/definitions.h 23:8@

    __exported by:__ @declarations\/definitions.h@
-}
data X = X
  { x_n :: BG.CInt
    {- ^ __C declaration:__ @n@

         __defined at:__ @declarations\/definitions.h 23:16@

         __exported by:__ @declarations\/definitions.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize X where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw X where

  readRaw =
    \ptr0 ->
          pure X
      <*> HasCField.readRaw (BG.Proxy @"x_n") ptr0

instance Marshal.WriteRaw X where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          X x_n2 ->
            HasCField.writeRaw (BG.Proxy @"x_n") ptr0 x_n2

deriving via Marshal.EquivStorable X instance BG.Storable X

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "x_n" X ty where

  hasField =
    \x0 -> (\y1 -> X {x_n = y1}, BG.getField @"x_n" x0)

instance (ty ~ BG.CInt) => BG.HasField "x_n" (BG.Ptr X) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"x_n")

instance HasCField.HasCField X "x_n" where

  type CFieldType X "x_n" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union Y@

    __defined at:__ @declarations\/definitions.h 26:7@

    __exported by:__ @declarations\/definitions.h@
-}
newtype Y = Y
  { unwrapY :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize Y

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw Y

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw Y

deriving via Marshal.EquivStorable Y instance BG.Storable Y

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion Y

{-| __C declaration:__ @m@

    __defined at:__ @declarations\/definitions.h 26:15@

    __exported by:__ @declarations\/definitions.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "y_m" Y ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @m@

    __defined at:__ @declarations\/definitions.h 26:15@

    __exported by:__ @declarations\/definitions.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "y_m" Y ty where

  hasField =
    \x0 -> (BG.setUnionPayload, BG.getField @"y_m" x0)

instance (ty ~ BG.CInt) => BG.HasField "y_m" (BG.Ptr Y) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"y_m")

instance HasCField.HasCField Y "y_m" where

  type CFieldType Y "y_m" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @o@

    __defined at:__ @declarations\/definitions.h 26:22@

    __exported by:__ @declarations\/definitions.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "y_o" Y ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @o@

    __defined at:__ @declarations\/definitions.h 26:22@

    __exported by:__ @declarations\/definitions.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "y_o" Y ty where

  hasField =
    \x0 -> (BG.setUnionPayload, BG.getField @"y_o" x0)

instance (ty ~ BG.CInt) => BG.HasField "y_o" (BG.Ptr Y) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"y_o")

instance HasCField.HasCField Y "y_o" where

  type CFieldType Y "y_o" = BG.CInt

  offset# = \_ -> \_ -> 0
