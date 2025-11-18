{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Prelude
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @foo@

    __defined at:__ @types\/primitives\/fixedwidth.h:3:8@

    __exported by:__ @types\/primitives\/fixedwidth.h@
-}
data Foo = Foo
  { foo_sixty_four :: HsBindgen.Runtime.Prelude.Word64
    {- ^ __C declaration:__ @sixty_four@

         __defined at:__ @types\/primitives\/fixedwidth.h:4:11@

         __exported by:__ @types\/primitives\/fixedwidth.h@
    -}
  , foo_thirty_two :: HsBindgen.Runtime.Prelude.Word32
    {- ^ __C declaration:__ @thirty_two@

         __defined at:__ @types\/primitives\/fixedwidth.h:5:11@

         __exported by:__ @types\/primitives\/fixedwidth.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Foo where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Foo
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"foo_sixty_four") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"foo_thirty_two") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_sixty_four2 foo_thirty_two3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"foo_sixty_four") ptr0 foo_sixty_four2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"foo_thirty_two") ptr0 foo_thirty_two3

instance HsBindgen.Runtime.HasCField.HasCField Foo "foo_sixty_four" where

  type CFieldType Foo "foo_sixty_four" =
    HsBindgen.Runtime.Prelude.Word64

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Foo) "foo_sixty_four")
         ) => GHC.Records.HasField "foo_sixty_four" (Ptr.Ptr Foo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"foo_sixty_four")

instance HsBindgen.Runtime.HasCField.HasCField Foo "foo_thirty_two" where

  type CFieldType Foo "foo_thirty_two" =
    HsBindgen.Runtime.Prelude.Word32

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Foo) "foo_thirty_two")
         ) => GHC.Records.HasField "foo_thirty_two" (Ptr.Ptr Foo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"foo_thirty_two")
