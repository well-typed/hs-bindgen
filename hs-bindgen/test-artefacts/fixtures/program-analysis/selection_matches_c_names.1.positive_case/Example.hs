{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.NewName(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct StructWithAssignedHaskellNameByPrescriptiveBindingSpecs@

    __defined at:__ @program-analysis\/selection_matches_c_names.h 7:8@

    __exported by:__ @program-analysis\/selection_matches_c_names.h@
-}
data NewName = NewName
  { newName_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @program-analysis\/selection_matches_c_names.h 8:7@

         __exported by:__ @program-analysis\/selection_matches_c_names.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize NewName where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw NewName where

  readRaw =
    \ptr0 ->
          pure NewName
      <*> HasCField.readRaw (BG.Proxy @"newName_x") ptr0

instance Marshal.WriteRaw NewName where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          NewName newName_x2 ->
            HasCField.writeRaw (BG.Proxy @"newName_x") ptr0 newName_x2

deriving via Marshal.EquivStorable NewName instance BG.Storable NewName

{-| __C declaration:__ @x@

    __defined at:__ @program-analysis\/selection_matches_c_names.h 8:7@

    __exported by:__ @program-analysis\/selection_matches_c_names.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "newName_x" NewName ty where

  hasField =
    \x0 ->
      (\y1 ->
         NewName {newName_x = y1}, BG.getField @"newName_x" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "newName_x" (BG.Ptr NewName) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"newName_x")

instance HasCField.HasCField NewName "newName_x" where

  type CFieldType NewName "newName_x" = BG.CInt

  offset# = \_ -> \_ -> 0
