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
    ( Example.Omitted(..)
    , Example.DirectlyDependsOnOmitted(..)
    , Example.IndirectlyDependsOnOmitted(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct Omitted@

    __defined at:__ @selection_omit_external_root.h 1:8@

    __exported by:__ @program-analysis\/selection_omit_external_b.h@
-}
data Omitted = Omitted
  { omitted_n :: BG.CInt
    {- ^ __C declaration:__ @n@

         __defined at:__ @selection_omit_external_root.h 2:7@

         __exported by:__ @program-analysis\/selection_omit_external_b.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Omitted where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Omitted where

  readRaw =
    \ptr0 ->
          pure Omitted
      <*> HasCField.readRaw (BG.Proxy @"omitted_n") ptr0

instance Marshal.WriteRaw Omitted where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Omitted omitted_n2 ->
            HasCField.writeRaw (BG.Proxy @"omitted_n") ptr0 omitted_n2

deriving via Marshal.EquivStorable Omitted instance BG.Storable Omitted

{-| __C declaration:__ @n@

    __defined at:__ @selection_omit_external_root.h 2:7@

    __exported by:__ @program-analysis\/selection_omit_external_b.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "omitted_n" Omitted ty where

  hasField =
    \x0 ->
      (\y1 ->
         Omitted {omitted_n = y1}, BG.getField @"omitted_n" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "omitted_n" (BG.Ptr Omitted) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"omitted_n")

instance HasCField.HasCField Omitted "omitted_n" where

  type CFieldType Omitted "omitted_n" = BG.CInt

  offset# = \_ -> \_ -> 0

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
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize DirectlyDependsOnOmitted where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw DirectlyDependsOnOmitted where

  readRaw =
    \ptr0 ->
          pure DirectlyDependsOnOmitted
      <*> HasCField.readRaw (BG.Proxy @"directlyDependsOnOmitted_o") ptr0

instance Marshal.WriteRaw DirectlyDependsOnOmitted where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          DirectlyDependsOnOmitted directlyDependsOnOmitted_o2 ->
            HasCField.writeRaw (BG.Proxy @"directlyDependsOnOmitted_o") ptr0 directlyDependsOnOmitted_o2

deriving via Marshal.EquivStorable DirectlyDependsOnOmitted instance BG.Storable DirectlyDependsOnOmitted

{-| __C declaration:__ @o@

    __defined at:__ @program-analysis\/selection_omit_external_b.h 5:18@

    __exported by:__ @program-analysis\/selection_omit_external_b.h@
-}
instance ( ty ~ Omitted
         ) => BG.CompatHasField.HasField "directlyDependsOnOmitted_o" DirectlyDependsOnOmitted ty where

  hasField =
    \x0 ->
      ( \y1 ->
          DirectlyDependsOnOmitted {directlyDependsOnOmitted_o = y1}
      , BG.getField @"directlyDependsOnOmitted_o" x0
      )

instance ( ty ~ Omitted
         ) => BG.HasField "directlyDependsOnOmitted_o" (BG.Ptr DirectlyDependsOnOmitted) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"directlyDependsOnOmitted_o")

instance HasCField.HasCField DirectlyDependsOnOmitted "directlyDependsOnOmitted_o" where

  type CFieldType DirectlyDependsOnOmitted "directlyDependsOnOmitted_o" =
    Omitted

  offset# = \_ -> \_ -> 0

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
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize IndirectlyDependsOnOmitted where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw IndirectlyDependsOnOmitted where

  readRaw =
    \ptr0 ->
          pure IndirectlyDependsOnOmitted
      <*> HasCField.readRaw (BG.Proxy @"indirectlyDependsOnOmitted_d") ptr0

instance Marshal.WriteRaw IndirectlyDependsOnOmitted where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          IndirectlyDependsOnOmitted indirectlyDependsOnOmitted_d2 ->
            HasCField.writeRaw (BG.Proxy @"indirectlyDependsOnOmitted_d") ptr0 indirectlyDependsOnOmitted_d2

deriving via Marshal.EquivStorable IndirectlyDependsOnOmitted instance BG.Storable IndirectlyDependsOnOmitted

{-| __C declaration:__ @d@

    __defined at:__ @program-analysis\/selection_omit_external_b.h 9:35@

    __exported by:__ @program-analysis\/selection_omit_external_b.h@
-}
instance ( ty ~ DirectlyDependsOnOmitted
         ) => BG.CompatHasField.HasField "indirectlyDependsOnOmitted_d" IndirectlyDependsOnOmitted ty where

  hasField =
    \x0 ->
      ( \y1 ->
          IndirectlyDependsOnOmitted {indirectlyDependsOnOmitted_d = y1}
      , BG.getField @"indirectlyDependsOnOmitted_d" x0
      )

instance ( ty ~ DirectlyDependsOnOmitted
         ) => BG.HasField "indirectlyDependsOnOmitted_d" (BG.Ptr IndirectlyDependsOnOmitted) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"indirectlyDependsOnOmitted_d")

instance HasCField.HasCField IndirectlyDependsOnOmitted "indirectlyDependsOnOmitted_d" where

  type CFieldType IndirectlyDependsOnOmitted "indirectlyDependsOnOmitted_d" =
    DirectlyDependsOnOmitted

  offset# = \_ -> \_ -> 0
