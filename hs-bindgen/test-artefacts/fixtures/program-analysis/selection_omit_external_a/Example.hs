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
    ( Example.UnrelatedDeclaration(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct UnrelatedDeclaration@

    __defined at:__ @program-analysis\/selection_omit_external_a.h 4:8@

    __exported by:__ @program-analysis\/selection_omit_external_a.h@
-}
data UnrelatedDeclaration = UnrelatedDeclaration
  { unrelatedDeclaration_m :: BG.CInt
    {- ^ __C declaration:__ @m@

         __defined at:__ @program-analysis\/selection_omit_external_a.h 5:7@

         __exported by:__ @program-analysis\/selection_omit_external_a.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize UnrelatedDeclaration where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw UnrelatedDeclaration where

  readRaw =
    \ptr0 ->
          pure UnrelatedDeclaration
      <*> HasCField.readRaw (BG.Proxy @"unrelatedDeclaration_m") ptr0

instance Marshal.WriteRaw UnrelatedDeclaration where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          UnrelatedDeclaration unrelatedDeclaration_m2 ->
            HasCField.writeRaw (BG.Proxy @"unrelatedDeclaration_m") ptr0 unrelatedDeclaration_m2

deriving via Marshal.EquivStorable UnrelatedDeclaration instance BG.Storable UnrelatedDeclaration

deriving via Struct.IsStructViaStorable UnrelatedDeclaration instance Struct.IsStruct UnrelatedDeclaration

{-| __C declaration:__ @m@

    __defined at:__ @program-analysis\/selection_omit_external_a.h 5:7@

    __exported by:__ @program-analysis\/selection_omit_external_a.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "unrelatedDeclaration_m" UnrelatedDeclaration ty where

  hasField =
    \x0 ->
      ( \y1 ->
          UnrelatedDeclaration {unrelatedDeclaration_m = y1}
      , BG.getField @"unrelatedDeclaration_m" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "unrelatedDeclaration_m" (BG.Ptr UnrelatedDeclaration) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unrelatedDeclaration_m")

instance HasCField.HasCField UnrelatedDeclaration "unrelatedDeclaration_m" where

  type CFieldType UnrelatedDeclaration "unrelatedDeclaration_m" =
    BG.CInt

  offset# = \_ -> \_ -> 0
