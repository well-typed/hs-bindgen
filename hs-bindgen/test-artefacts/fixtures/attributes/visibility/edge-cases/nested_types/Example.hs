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
    ( Example.U(..)
    , Example.S(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @union U@

    __defined at:__ @attributes\/visibility\/edge-cases\/nested_types.h 3:49@

    __exported by:__ @attributes\/visibility\/edge-cases\/nested_types.h@
-}
newtype U = U
  { unwrapU :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize U

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw U

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw U

deriving via Marshal.EquivStorable U instance BG.Storable U

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion U

{-| __C declaration:__ @x@

    __defined at:__ @attributes\/visibility\/edge-cases\/nested_types.h 4:9@

    __exported by:__ @attributes\/visibility\/edge-cases\/nested_types.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "u_x" U ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @attributes\/visibility\/edge-cases\/nested_types.h 4:9@

    __exported by:__ @attributes\/visibility\/edge-cases\/nested_types.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "u_x" U ty where

  hasField =
    \x0 -> (BG.setUnionPayload, BG.getField @"u_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "u_x" (BG.Ptr U) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"u_x")

instance HasCField.HasCField U "u_x" where

  type CFieldType U "u_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct S@

    __defined at:__ @attributes\/visibility\/edge-cases\/nested_types.h 2:49@

    __exported by:__ @attributes\/visibility\/edge-cases\/nested_types.h@
-}
data S = S
  { s_y :: U
    {- ^ __C declaration:__ @y@

         __defined at:__ @attributes\/visibility\/edge-cases\/nested_types.h 5:5@

         __exported by:__ @attributes\/visibility\/edge-cases\/nested_types.h@
    -}
  }
  deriving stock (BG.Generic)

instance Marshal.StaticSize S where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S where

  readRaw =
    \ptr0 ->
          pure S
      <*> HasCField.readRaw (BG.Proxy @"s_y") ptr0

instance Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_y2 ->
            HasCField.writeRaw (BG.Proxy @"s_y") ptr0 s_y2

deriving via Marshal.EquivStorable S instance BG.Storable S

instance (ty ~ U) => BG.CompatHasField.HasField "s_y" S ty where

  hasField =
    \x0 -> (\y1 -> S {s_y = y1}, BG.getField @"s_y" x0)

instance (ty ~ U) => BG.HasField "s_y" (BG.Ptr S) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s_y")

instance HasCField.HasCField S "s_y" where

  type CFieldType S "s_y" = U

  offset# = \_ -> \_ -> 0
