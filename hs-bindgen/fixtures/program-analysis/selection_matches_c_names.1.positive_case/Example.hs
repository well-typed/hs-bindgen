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

{-| __C declaration:__ @struct StructWithAssignedHaskellNameByPrescriptiveBindingSpecs@

    __defined at:__ @program-analysis\/selection_matches_c_names.h 7:8@

    __exported by:__ @program-analysis\/selection_matches_c_names.h@
-}
data NewName = NewName
  { newName_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @program-analysis\/selection_matches_c_names.h 8:7@

         __exported by:__ @program-analysis\/selection_matches_c_names.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize NewName where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw NewName where

  readRaw =
    \ptr0 ->
          pure NewName
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"newName_x") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw NewName where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          NewName newName_x2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"newName_x") ptr0 newName_x2

deriving via HsBindgen.Runtime.Marshal.EquivStorable NewName instance F.Storable NewName

instance HsBindgen.Runtime.HasCField.HasCField NewName "newName_x" where

  type CFieldType NewName "newName_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType NewName) "newName_x")
         ) => GHC.Records.HasField "newName_x" (Ptr.Ptr NewName) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"newName_x")
