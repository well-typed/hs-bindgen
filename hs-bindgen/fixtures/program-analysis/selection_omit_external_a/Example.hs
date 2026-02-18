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

{-| __C declaration:__ @struct UnrelatedDeclaration@

    __defined at:__ @program-analysis\/selection_omit_external_a.h 4:8@

    __exported by:__ @program-analysis\/selection_omit_external_a.h@
-}
data UnrelatedDeclaration = UnrelatedDeclaration
  { unrelatedDeclaration_m :: RIP.CInt
    {- ^ __C declaration:__ @m@

         __defined at:__ @program-analysis\/selection_omit_external_a.h 5:7@

         __exported by:__ @program-analysis\/selection_omit_external_a.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize UnrelatedDeclaration where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw UnrelatedDeclaration where

  readRaw =
    \ptr0 ->
          pure UnrelatedDeclaration
      <*> HasCField.readRaw (RIP.Proxy @"unrelatedDeclaration_m") ptr0

instance Marshal.WriteRaw UnrelatedDeclaration where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          UnrelatedDeclaration unrelatedDeclaration_m2 ->
            HasCField.writeRaw (RIP.Proxy @"unrelatedDeclaration_m") ptr0 unrelatedDeclaration_m2

deriving via Marshal.EquivStorable UnrelatedDeclaration instance RIP.Storable UnrelatedDeclaration

instance HasCField.HasCField UnrelatedDeclaration "unrelatedDeclaration_m" where

  type CFieldType UnrelatedDeclaration "unrelatedDeclaration_m" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unrelatedDeclaration_m" (RIP.Ptr UnrelatedDeclaration) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unrelatedDeclaration_m")
