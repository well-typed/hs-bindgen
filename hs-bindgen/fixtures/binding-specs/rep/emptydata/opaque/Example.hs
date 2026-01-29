{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
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

{-| __C declaration:__ @struct foo@

    __defined at:__ @binding-specs\/rep\/emptydata\/opaque.h 1:8@

    __exported by:__ @binding-specs\/rep\/emptydata\/opaque.h@
-}
data Foo

{-| __C declaration:__ @struct bar@

    __defined at:__ @binding-specs\/rep\/emptydata\/opaque.h 3:8@

    __exported by:__ @binding-specs\/rep\/emptydata\/opaque.h@
-}
data Bar = Bar
  { bar_a :: Ptr.Ptr Foo
    {- ^ __C declaration:__ @a@

         __defined at:__ @binding-specs\/rep\/emptydata\/opaque.h 4:15@

         __exported by:__ @binding-specs\/rep\/emptydata\/opaque.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Bar where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Bar
      <*> HsBindgen.Runtime.HasCField.peek (Data.Proxy.Proxy @"bar_a") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_a2 ->
            HsBindgen.Runtime.HasCField.poke (Data.Proxy.Proxy @"bar_a") ptr0 bar_a2

instance HsBindgen.Runtime.HasCField.HasCField Bar "bar_a" where

  type CFieldType Bar "bar_a" = Ptr.Ptr Foo

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Bar) "bar_a")
         ) => GHC.Records.HasField "bar_a" (Ptr.Ptr Bar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"bar_a")
