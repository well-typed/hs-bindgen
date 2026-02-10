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
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Marshal
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct UnrelatedDeclaration@

    __defined at:__ @program-analysis\/selection_omit_external_a.h 4:8@

    __exported by:__ @program-analysis\/selection_omit_external_a.h@
-}
data UnrelatedDeclaration = UnrelatedDeclaration
  { unrelatedDeclaration_m :: FC.CInt
    {- ^ __C declaration:__ @m@

         __defined at:__ @program-analysis\/selection_omit_external_a.h 5:7@

         __exported by:__ @program-analysis\/selection_omit_external_a.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize UnrelatedDeclaration where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw UnrelatedDeclaration where

  readRaw =
    \ptr0 ->
          pure UnrelatedDeclaration
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"unrelatedDeclaration_m") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw UnrelatedDeclaration where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          UnrelatedDeclaration unrelatedDeclaration_m2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"unrelatedDeclaration_m") ptr0 unrelatedDeclaration_m2

deriving via HsBindgen.Runtime.Marshal.EquivStorable UnrelatedDeclaration instance F.Storable UnrelatedDeclaration

instance HsBindgen.Runtime.HasCField.HasCField UnrelatedDeclaration "unrelatedDeclaration_m" where

  type CFieldType UnrelatedDeclaration "unrelatedDeclaration_m" =
    FC.CInt

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "unrelatedDeclaration_m" (Ptr.Ptr UnrelatedDeclaration) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unrelatedDeclaration_m")
