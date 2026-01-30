{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct S@

    __defined at:__ @functions\/heap_types\/struct_const.h 3:8@

    __exported by:__ @functions\/heap_types\/struct_const.h@
-}
data T = T
  { t_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @functions\/heap_types\/struct_const.h 4:7@

         __exported by:__ @functions\/heap_types\/struct_const.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize T where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw T where

  readRaw =
    \ptr0 ->
          pure T
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"t_x") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw T where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T t_x2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"t_x") ptr0 t_x2

deriving via HsBindgen.Runtime.Marshal.EquivStorable T instance F.Storable T

instance HsBindgen.Runtime.HasCField.HasCField T "t_x" where

  type CFieldType T "t_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType T) "t_x")
         ) => GHC.Records.HasField "t_x" (Ptr.Ptr T) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"t_x")
