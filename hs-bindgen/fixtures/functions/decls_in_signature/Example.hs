{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
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

{-| __C declaration:__ @struct opaque@

    __defined at:__ @functions\/decls_in_signature.h 2:8@

    __exported by:__ @functions\/decls_in_signature.h@
-}
data Opaque

{-| __C declaration:__ @struct outside@

    __defined at:__ @functions\/decls_in_signature.h 3:8@

    __exported by:__ @functions\/decls_in_signature.h@
-}
data Outside = Outside
  { outside_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @functions\/decls_in_signature.h 4:7@

         __exported by:__ @functions\/decls_in_signature.h@
    -}
  , outside_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @functions\/decls_in_signature.h 5:7@

         __exported by:__ @functions\/decls_in_signature.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Outside where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Outside where

  readRaw =
    \ptr0 ->
          pure Outside
      <*> HasCField.readRaw (RIP.Proxy @"outside_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"outside_y") ptr0

instance Marshal.WriteRaw Outside where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Outside outside_x2 outside_y3 ->
               HasCField.writeRaw (RIP.Proxy @"outside_x") ptr0 outside_x2
            >> HasCField.writeRaw (RIP.Proxy @"outside_y") ptr0 outside_y3

deriving via Marshal.EquivStorable Outside instance RIP.Storable Outside

instance HasCField.HasCField Outside "outside_x" where

  type CFieldType Outside "outside_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "outside_x" (RIP.Ptr Outside) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"outside_x")

instance HasCField.HasCField Outside "outside_y" where

  type CFieldType Outside "outside_y" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "outside_y" (RIP.Ptr Outside) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"outside_y")

{-| Error cases

  See 'UnexpectedAnonInSignature' for discussion (of both these error cases and the edge cases below).

__C declaration:__ @struct named_struct@

__defined at:__ @functions\/decls_in_signature.h 17:16@

__exported by:__ @functions\/decls_in_signature.h@
-}
data Named_struct = Named_struct
  { named_struct_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @functions\/decls_in_signature.h 17:35@

         __exported by:__ @functions\/decls_in_signature.h@
    -}
  , named_struct_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @functions\/decls_in_signature.h 17:42@

         __exported by:__ @functions\/decls_in_signature.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Named_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Named_struct where

  readRaw =
    \ptr0 ->
          pure Named_struct
      <*> HasCField.readRaw (RIP.Proxy @"named_struct_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"named_struct_y") ptr0

instance Marshal.WriteRaw Named_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Named_struct named_struct_x2 named_struct_y3 ->
               HasCField.writeRaw (RIP.Proxy @"named_struct_x") ptr0 named_struct_x2
            >> HasCField.writeRaw (RIP.Proxy @"named_struct_y") ptr0 named_struct_y3

deriving via Marshal.EquivStorable Named_struct instance RIP.Storable Named_struct

instance HasCField.HasCField Named_struct "named_struct_x" where

  type CFieldType Named_struct "named_struct_x" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "named_struct_x" (RIP.Ptr Named_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"named_struct_x")

instance HasCField.HasCField Named_struct "named_struct_y" where

  type CFieldType Named_struct "named_struct_y" =
    RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "named_struct_y" (RIP.Ptr Named_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"named_struct_y")

{-| __C declaration:__ @union named_union@

    __defined at:__ @functions\/decls_in_signature.h 20:15@

    __exported by:__ @functions\/decls_in_signature.h@
-}
newtype Named_union = Named_union
  { unwrapNamed_union :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize Named_union

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw Named_union

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw Named_union

deriving via Marshal.EquivStorable Named_union instance RIP.Storable Named_union

{-|

  __See:__ 'set_named_union_x'

__C declaration:__ @x@

__defined at:__ @functions\/decls_in_signature.h 20:33@

__exported by:__ @functions\/decls_in_signature.h@
-}
get_named_union_x ::
     Named_union
  -> RIP.CInt
get_named_union_x = RIP.getUnionPayload

{-|

  __See:__ 'get_named_union_x'

-}
set_named_union_x ::
     RIP.CInt
  -> Named_union
set_named_union_x = RIP.setUnionPayload

{-|

  __See:__ 'set_named_union_y'

__C declaration:__ @y@

__defined at:__ @functions\/decls_in_signature.h 20:41@

__exported by:__ @functions\/decls_in_signature.h@
-}
get_named_union_y ::
     Named_union
  -> RIP.CChar
get_named_union_y = RIP.getUnionPayload

{-|

  __See:__ 'get_named_union_y'

-}
set_named_union_y ::
     RIP.CChar
  -> Named_union
set_named_union_y = RIP.setUnionPayload

instance HasCField.HasCField Named_union "named_union_x" where

  type CFieldType Named_union "named_union_x" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "named_union_x" (RIP.Ptr Named_union) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"named_union_x")

instance HasCField.HasCField Named_union "named_union_y" where

  type CFieldType Named_union "named_union_y" =
    RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "named_union_y" (RIP.Ptr Named_union) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"named_union_y")
