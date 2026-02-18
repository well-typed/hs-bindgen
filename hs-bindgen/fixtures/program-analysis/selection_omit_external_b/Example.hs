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

{-| __C declaration:__ @struct Omitted@

    __defined at:__ @selection_omit_external_root.h 1:8@

    __exported by:__ @program-analysis\/selection_omit_external_b.h@
-}
data Omitted = Omitted
  { omitted_n :: RIP.CInt
    {- ^ __C declaration:__ @n@

         __defined at:__ @selection_omit_external_root.h 2:7@

         __exported by:__ @program-analysis\/selection_omit_external_b.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Omitted where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Omitted where

  readRaw =
    \ptr0 ->
          pure Omitted
      <*> HasCField.readRaw (RIP.Proxy @"omitted_n") ptr0

instance Marshal.WriteRaw Omitted where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Omitted omitted_n2 ->
            HasCField.writeRaw (RIP.Proxy @"omitted_n") ptr0 omitted_n2

deriving via Marshal.EquivStorable Omitted instance RIP.Storable Omitted

instance HasCField.HasCField Omitted "omitted_n" where

  type CFieldType Omitted "omitted_n" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "omitted_n" (RIP.Ptr Omitted) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"omitted_n")

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
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize DirectlyDependsOnOmitted where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw DirectlyDependsOnOmitted where

  readRaw =
    \ptr0 ->
          pure DirectlyDependsOnOmitted
      <*> HasCField.readRaw (RIP.Proxy @"directlyDependsOnOmitted_o") ptr0

instance Marshal.WriteRaw DirectlyDependsOnOmitted where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          DirectlyDependsOnOmitted directlyDependsOnOmitted_o2 ->
            HasCField.writeRaw (RIP.Proxy @"directlyDependsOnOmitted_o") ptr0 directlyDependsOnOmitted_o2

deriving via Marshal.EquivStorable DirectlyDependsOnOmitted instance RIP.Storable DirectlyDependsOnOmitted

instance HasCField.HasCField DirectlyDependsOnOmitted "directlyDependsOnOmitted_o" where

  type CFieldType DirectlyDependsOnOmitted "directlyDependsOnOmitted_o" =
    Omitted

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Omitted
         ) => RIP.HasField "directlyDependsOnOmitted_o" (RIP.Ptr DirectlyDependsOnOmitted) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"directlyDependsOnOmitted_o")

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
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize IndirectlyDependsOnOmitted where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw IndirectlyDependsOnOmitted where

  readRaw =
    \ptr0 ->
          pure IndirectlyDependsOnOmitted
      <*> HasCField.readRaw (RIP.Proxy @"indirectlyDependsOnOmitted_d") ptr0

instance Marshal.WriteRaw IndirectlyDependsOnOmitted where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          IndirectlyDependsOnOmitted indirectlyDependsOnOmitted_d2 ->
            HasCField.writeRaw (RIP.Proxy @"indirectlyDependsOnOmitted_d") ptr0 indirectlyDependsOnOmitted_d2

deriving via Marshal.EquivStorable IndirectlyDependsOnOmitted instance RIP.Storable IndirectlyDependsOnOmitted

instance HasCField.HasCField IndirectlyDependsOnOmitted "indirectlyDependsOnOmitted_d" where

  type CFieldType IndirectlyDependsOnOmitted "indirectlyDependsOnOmitted_d" =
    DirectlyDependsOnOmitted

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) DirectlyDependsOnOmitted
         ) => RIP.HasField "indirectlyDependsOnOmitted_d" (RIP.Ptr IndirectlyDependsOnOmitted) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"indirectlyDependsOnOmitted_d")
