{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Array.Byte
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.ByteArray
import qualified HsBindgen.Runtime.Internal.SizedByteArray
import qualified HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

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
  { outside_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @functions\/decls_in_signature.h 4:7@

         __exported by:__ @functions\/decls_in_signature.h@
    -}
  , outside_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @functions\/decls_in_signature.h 5:7@

         __exported by:__ @functions\/decls_in_signature.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Outside where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Outside where

  readRaw =
    \ptr0 ->
          pure Outside
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"outside_x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"outside_y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Outside where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Outside outside_x2 outside_y3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"outside_x") ptr0 outside_x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"outside_y") ptr0 outside_y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Outside instance F.Storable Outside

instance HsBindgen.Runtime.HasCField.HasCField Outside "outside_x" where

  type CFieldType Outside "outside_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "outside_x" (Ptr.Ptr Outside) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"outside_x")

instance HsBindgen.Runtime.HasCField.HasCField Outside "outside_y" where

  type CFieldType Outside "outside_y" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "outside_y" (Ptr.Ptr Outside) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"outside_y")

{-| Error cases

  See 'UnexpectedAnonInSignature' for discussion (of both these error cases and the edge cases below).

__C declaration:__ @struct named_struct@

__defined at:__ @functions\/decls_in_signature.h 17:16@

__exported by:__ @functions\/decls_in_signature.h@
-}
data Named_struct = Named_struct
  { named_struct_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @functions\/decls_in_signature.h 17:35@

         __exported by:__ @functions\/decls_in_signature.h@
    -}
  , named_struct_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @functions\/decls_in_signature.h 17:42@

         __exported by:__ @functions\/decls_in_signature.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Named_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Named_struct where

  readRaw =
    \ptr0 ->
          pure Named_struct
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"named_struct_x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"named_struct_y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Named_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Named_struct named_struct_x2 named_struct_y3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"named_struct_x") ptr0 named_struct_x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"named_struct_y") ptr0 named_struct_y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Named_struct instance F.Storable Named_struct

instance HsBindgen.Runtime.HasCField.HasCField Named_struct "named_struct_x" where

  type CFieldType Named_struct "named_struct_x" =
    FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "named_struct_x" (Ptr.Ptr Named_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"named_struct_x")

instance HsBindgen.Runtime.HasCField.HasCField Named_struct "named_struct_y" where

  type CFieldType Named_struct "named_struct_y" =
    FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "named_struct_y" (Ptr.Ptr Named_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"named_struct_y")

{-| __C declaration:__ @union named_union@

    __defined at:__ @functions\/decls_in_signature.h 20:15@

    __exported by:__ @functions\/decls_in_signature.h@
-}
newtype Named_union = Named_union
  { unwrapNamed_union :: Data.Array.Byte.ByteArray
  }
  deriving stock (GHC.Generics.Generic)

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 4) 4 instance HsBindgen.Runtime.Marshal.StaticSize Named_union

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 4) 4 instance HsBindgen.Runtime.Marshal.ReadRaw Named_union

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 4) 4 instance HsBindgen.Runtime.Marshal.WriteRaw Named_union

deriving via HsBindgen.Runtime.Marshal.EquivStorable Named_union instance F.Storable Named_union

{-|

  __See:__ 'set_named_union_x'

__C declaration:__ @x@

__defined at:__ @functions\/decls_in_signature.h 20:33@

__exported by:__ @functions\/decls_in_signature.h@
-}
get_named_union_x ::
     Named_union
  -> FC.CInt
get_named_union_x =
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_named_union_x'

-}
set_named_union_x ::
     FC.CInt
  -> Named_union
set_named_union_x =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

{-|

  __See:__ 'set_named_union_y'

__C declaration:__ @y@

__defined at:__ @functions\/decls_in_signature.h 20:41@

__exported by:__ @functions\/decls_in_signature.h@
-}
get_named_union_y ::
     Named_union
  -> FC.CChar
get_named_union_y =
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_named_union_y'

-}
set_named_union_y ::
     FC.CChar
  -> Named_union
set_named_union_y =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField Named_union "named_union_x" where

  type CFieldType Named_union "named_union_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "named_union_x" (Ptr.Ptr Named_union) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"named_union_x")

instance HsBindgen.Runtime.HasCField.HasCField Named_union "named_union_y" where

  type CFieldType Named_union "named_union_y" =
    FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty FC.CChar
         ) => GHC.Records.HasField "named_union_y" (Ptr.Ptr Named_union) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"named_union_y")
