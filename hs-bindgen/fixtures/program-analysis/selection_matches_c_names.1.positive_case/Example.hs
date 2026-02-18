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
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct StructWithAssignedHaskellNameByPrescriptiveBindingSpecs@

    __defined at:__ @program-analysis\/selection_matches_c_names.h 7:8@

    __exported by:__ @program-analysis\/selection_matches_c_names.h@
-}
data NewName = NewName
  { newName_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @program-analysis\/selection_matches_c_names.h 8:7@

         __exported by:__ @program-analysis\/selection_matches_c_names.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize NewName where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw NewName where

  readRaw =
    \ptr0 ->
          pure NewName
      <*> HasCField.readRaw (RIP.Proxy @"newName_x") ptr0

instance Marshal.WriteRaw NewName where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          NewName newName_x2 ->
            HasCField.writeRaw (RIP.Proxy @"newName_x") ptr0 newName_x2

deriving via Marshal.EquivStorable NewName instance RIP.Storable NewName

instance HasCField.HasCField NewName "newName_x" where

  type CFieldType NewName "newName_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "newName_x" (RIP.Ptr NewName) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"newName_x")
