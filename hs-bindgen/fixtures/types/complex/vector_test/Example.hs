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
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __defined at:__ @types\/complex\/vector_test.h:1:9@

    __exported by:__ @types\/complex\/vector_test.h@
-}
data Vector = Vector
  { vector_x :: FC.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/complex\/vector_test.h:2:12@

         __exported by:__ @types\/complex\/vector_test.h@
    -}
  , vector_y :: FC.CDouble
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/complex\/vector_test.h:3:12@

         __exported by:__ @types\/complex\/vector_test.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Vector where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Vector
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"vector_x") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"vector_y") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Vector vector_x2 vector_y3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"vector_x") ptr0 vector_x2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"vector_y") ptr0 vector_y3

instance HsBindgen.Runtime.HasCField.HasCField Vector "vector_x" where

  type CFieldType Vector "vector_x" = FC.CDouble

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Vector) "vector_x")
         ) => GHC.Records.HasField "vector_x" (Ptr.Ptr Vector) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"vector_x")

instance HsBindgen.Runtime.HasCField.HasCField Vector "vector_y" where

  type CFieldType Vector "vector_y" = FC.CDouble

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Vector) "vector_y")
         ) => GHC.Records.HasField "vector_y" (Ptr.Ptr Vector) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"vector_y")
