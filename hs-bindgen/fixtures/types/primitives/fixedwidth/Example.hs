{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct foo@

    __defined at:__ @types\/primitives\/fixedwidth.h 3:8@

    __exported by:__ @types\/primitives\/fixedwidth.h@
-}
data Foo = Foo
  { foo_sixty_four :: HsBindgen.Runtime.LibC.Word64
    {- ^ __C declaration:__ @sixty_four@

         __defined at:__ @types\/primitives\/fixedwidth.h 4:11@

         __exported by:__ @types\/primitives\/fixedwidth.h@
    -}
  , foo_thirty_two :: HsBindgen.Runtime.LibC.Word32
    {- ^ __C declaration:__ @thirty_two@

         __defined at:__ @types\/primitives\/fixedwidth.h 5:11@

         __exported by:__ @types\/primitives\/fixedwidth.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Foo where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Foo where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"foo_sixty_four") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"foo_thirty_two") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Foo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_sixty_four2 foo_thirty_two3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"foo_sixty_four") ptr0 foo_sixty_four2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"foo_thirty_two") ptr0 foo_thirty_two3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Foo instance F.Storable Foo

instance HsBindgen.Runtime.HasCField.HasCField Foo "foo_sixty_four" where

  type CFieldType Foo "foo_sixty_four" =
    HsBindgen.Runtime.LibC.Word64

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "foo_sixty_four" (Ptr.Ptr Foo) (Ptr.Ptr HsBindgen.Runtime.LibC.Word64) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"foo_sixty_four")

instance HsBindgen.Runtime.HasCField.HasCField Foo "foo_thirty_two" where

  type CFieldType Foo "foo_thirty_two" =
    HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 8

instance GHC.Records.HasField "foo_thirty_two" (Ptr.Ptr Foo) (Ptr.Ptr HsBindgen.Runtime.LibC.Word32) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"foo_thirty_two")
