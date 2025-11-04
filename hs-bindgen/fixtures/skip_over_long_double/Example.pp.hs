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
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct2@

    __defined at:__ @skip_over_long_double.h:13:8@

    __exported by:__ @skip_over_long_double.h@
-}
data Struct2 = Struct2
  { struct2_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @skip_over_long_double.h:14:7@

         __exported by:__ @skip_over_long_double.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct2 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct2
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"struct2_x") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2 struct2_x2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"struct2_x") ptr0 struct2_x2

instance HsBindgen.Runtime.HasCField.HasCField Struct2 "struct2_x" where

  type CFieldType Struct2 "struct2_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct2) "struct2_x")
         ) => GHC.Records.HasField "struct2_x" (Ptr.Ptr Struct2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"struct2_x")
