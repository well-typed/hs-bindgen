{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct Omitted@

    __defined at:__ @selection_omit_external_root.h 1:8@

    __exported by:__ @program-analysis\/selection_omit_external_b.h@
-}
data Omitted = Omitted
  { omitted_n :: FC.CInt
    {- ^ __C declaration:__ @n@

         __defined at:__ @selection_omit_external_root.h 2:7@

         __exported by:__ @program-analysis\/selection_omit_external_b.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Omitted where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Omitted where

  readRaw =
    \ptr0 ->
          pure Omitted
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"omitted_n") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Omitted where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Omitted omitted_n2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"omitted_n") ptr0 omitted_n2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Omitted instance F.Storable Omitted

instance HsBindgen.Runtime.HasCField.HasCField Omitted "omitted_n" where

  type CFieldType Omitted "omitted_n" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "omitted_n" (Ptr.Ptr Omitted) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"omitted_n")

{-| __C declaration:__ @struct DirectlyDependsOnOmitted@

    __defined at:__ @program-analysis\/selection_omit_external_b.h 4:8@

    __exported by:__ @program-analysis\/selection_omit_external_b.h@
-}
data DirectlyDependsOnOmitted = DirectlyDependsOnOmitted
  { directlyDependsOnOmitted_o :: Omitted
    {- ^ __C declaration:__ @o@

         __defined at:__ @program-analysis\/selection_omit_external_b.h 5:18@

         __exported by:__ @program-analysis\/selection_omit_external_b.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize DirectlyDependsOnOmitted where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw DirectlyDependsOnOmitted where

  readRaw =
    \ptr0 ->
          pure DirectlyDependsOnOmitted
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"directlyDependsOnOmitted_o") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw DirectlyDependsOnOmitted where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          DirectlyDependsOnOmitted directlyDependsOnOmitted_o2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"directlyDependsOnOmitted_o") ptr0 directlyDependsOnOmitted_o2

deriving via HsBindgen.Runtime.Marshal.EquivStorable DirectlyDependsOnOmitted instance F.Storable DirectlyDependsOnOmitted

instance HsBindgen.Runtime.HasCField.HasCField DirectlyDependsOnOmitted "directlyDependsOnOmitted_o" where

  type CFieldType DirectlyDependsOnOmitted "directlyDependsOnOmitted_o" =
    Omitted

  offset# = \_ -> \_ -> 0

instance ( TyEq ty Omitted
         ) => GHC.Records.HasField "directlyDependsOnOmitted_o" (Ptr.Ptr DirectlyDependsOnOmitted) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"directlyDependsOnOmitted_o")

{-| __C declaration:__ @struct IndirectlyDependsOnOmitted@

    __defined at:__ @program-analysis\/selection_omit_external_b.h 8:8@

    __exported by:__ @program-analysis\/selection_omit_external_b.h@
-}
data IndirectlyDependsOnOmitted = IndirectlyDependsOnOmitted
  { indirectlyDependsOnOmitted_d :: DirectlyDependsOnOmitted
    {- ^ __C declaration:__ @d@

         __defined at:__ @program-analysis\/selection_omit_external_b.h 9:35@

         __exported by:__ @program-analysis\/selection_omit_external_b.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize IndirectlyDependsOnOmitted where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw IndirectlyDependsOnOmitted where

  readRaw =
    \ptr0 ->
          pure IndirectlyDependsOnOmitted
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"indirectlyDependsOnOmitted_d") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw IndirectlyDependsOnOmitted where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          IndirectlyDependsOnOmitted indirectlyDependsOnOmitted_d2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"indirectlyDependsOnOmitted_d") ptr0 indirectlyDependsOnOmitted_d2

deriving via HsBindgen.Runtime.Marshal.EquivStorable IndirectlyDependsOnOmitted instance F.Storable IndirectlyDependsOnOmitted

instance HsBindgen.Runtime.HasCField.HasCField IndirectlyDependsOnOmitted "indirectlyDependsOnOmitted_d" where

  type CFieldType IndirectlyDependsOnOmitted "indirectlyDependsOnOmitted_d" =
    DirectlyDependsOnOmitted

  offset# = \_ -> \_ -> 0

instance ( TyEq ty DirectlyDependsOnOmitted
         ) => GHC.Records.HasField "indirectlyDependsOnOmitted_d" (Ptr.Ptr IndirectlyDependsOnOmitted) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"indirectlyDependsOnOmitted_d")
