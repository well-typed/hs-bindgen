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
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @b@

    __defined at:__ @types\/structs\/circular_dependency_struct.h:3:8@

    __exported by:__ @types\/structs\/circular_dependency_struct.h@
-}
data B = B
  { b_toA :: Ptr.Ptr A
    {- ^ __C declaration:__ @toA@

         __defined at:__ @types\/structs\/circular_dependency_struct.h:4:13@

         __exported by:__ @types\/structs\/circular_dependency_struct.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable B where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure B
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"b_toA") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          B b_toA2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"b_toA") ptr0 b_toA2

instance HsBindgen.Runtime.HasCField.HasCField B "b_toA" where

  type CFieldType B "b_toA" = Ptr.Ptr A

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType B) "b_toA")
         ) => GHC.Records.HasField "b_toA" (Ptr.Ptr B) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"b_toA")

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/circular_dependency_struct.h:7:8@

    __exported by:__ @types\/structs\/circular_dependency_struct.h@
-}
data A = A
  { a_toB :: B
    {- ^ __C declaration:__ @toB@

         __defined at:__ @types\/structs\/circular_dependency_struct.h:8:12@

         __exported by:__ @types\/structs\/circular_dependency_struct.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable A where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure A
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"a_toB") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          A a_toB2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"a_toB") ptr0 a_toB2

instance HsBindgen.Runtime.HasCField.HasCField A "a_toB" where

  type CFieldType A "a_toB" = B

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A) "a_toB")
         ) => GHC.Records.HasField "a_toB" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"a_toB")
