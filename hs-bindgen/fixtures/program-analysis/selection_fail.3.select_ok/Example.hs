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

{-| __C declaration:__ @struct OkBefore@

    __defined at:__ @program-analysis\/selection_fail.h 1:8@

    __exported by:__ @program-analysis\/selection_fail.h@
-}
data OkBefore = OkBefore
  { okBefore_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @program-analysis\/selection_fail.h 2:7@

         __exported by:__ @program-analysis\/selection_fail.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize OkBefore where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw OkBefore where

  readRaw =
    \ptr0 ->
          pure OkBefore
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"okBefore_x") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw OkBefore where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          OkBefore okBefore_x2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"okBefore_x") ptr0 okBefore_x2

deriving via HsBindgen.Runtime.Marshal.EquivStorable OkBefore instance F.Storable OkBefore

instance HsBindgen.Runtime.HasCField.HasCField OkBefore "okBefore_x" where

  type CFieldType OkBefore "okBefore_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType OkBefore) "okBefore_x")
         ) => GHC.Records.HasField "okBefore_x" (Ptr.Ptr OkBefore) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"okBefore_x")
