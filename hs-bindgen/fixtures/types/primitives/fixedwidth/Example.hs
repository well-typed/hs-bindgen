{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal as Marshal

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
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Foo where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Foo where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> HasCField.readRaw (RIP.Proxy @"foo_sixty_four") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"foo_thirty_two") ptr0

instance Marshal.WriteRaw Foo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_sixty_four2 foo_thirty_two3 ->
               HasCField.writeRaw (RIP.Proxy @"foo_sixty_four") ptr0 foo_sixty_four2
            >> HasCField.writeRaw (RIP.Proxy @"foo_thirty_two") ptr0 foo_thirty_two3

deriving via Marshal.EquivStorable Foo instance RIP.Storable Foo

instance HasCField.HasCField Foo "foo_sixty_four" where

  type CFieldType Foo "foo_sixty_four" =
    HsBindgen.Runtime.LibC.Word64

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) HsBindgen.Runtime.LibC.Word64
         ) => RIP.HasField "foo_sixty_four" (RIP.Ptr Foo) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"foo_sixty_four")

instance HasCField.HasCField Foo "foo_thirty_two" where

  type CFieldType Foo "foo_thirty_two" =
    HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) HsBindgen.Runtime.LibC.Word32
         ) => RIP.HasField "foo_thirty_two" (RIP.Ptr Foo) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"foo_thirty_two")
