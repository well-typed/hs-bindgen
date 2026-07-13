{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Foo(..)
    , pattern Example.Bar
    , pattern Example.Baz
    , Example.ElementaryParticleType(..)
    , pattern Example.Quark_up
    , pattern Example.Quark_down
    , pattern Example.Quark_charm
    , pattern Example.Quark_strange
    , pattern Example.Quark_top
    , pattern Example.Quark_bottom
    , pattern Example.Lepton_electron
    , pattern Example.Lepton_electron_neutrino
    , pattern Example.Lepton_muon
    , pattern Example.Lepton_muon_neutrino
    , pattern Example.Lepton_tau
    , pattern Example.Lepton_tau_neutrino
    , pattern Example.Boson_gluon
    , pattern Example.Boson_photon
    , pattern Example.Boson_z
    , pattern Example.Boson_w
    , pattern Example.Boson_higgs
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @enum foo@

    __defined at:__ @binding-specs\/enum\/closed.h 2:6@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
newtype Foo = Foo
  { unwrapFoo :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize Foo where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Foo where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Foo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo unwrapFoo2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapFoo2

deriving via Marshal.EquivStorable Foo instance BG.Storable Foo

deriving via BG.CUInt instance BG.Prim Foo

instance CEnum.CEnum Foo where

  type CEnumZ Foo = BG.CUInt

  toCEnum = Foo

  fromCEnum = BG.getField @"unwrapFoo"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "Bar"), (1, BG.singleton "Baz")]

  showsUndeclared = CEnum.showsWrappedUndeclared "Foo"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Foo"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Foo where

  minDeclaredValue = Bar

  maxDeclaredValue = Baz

instance Show Foo where

  showsPrec = CEnum.shows

instance Read Foo where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance (ty ~ BG.CUInt) => BG.CompatHasField.HasField "unwrapFoo" Foo ty where

  hasField =
    \x0 ->
      (\y1 ->
         Foo {unwrapFoo = y1}, BG.getField @"unwrapFoo" x0)

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapFoo" (BG.Ptr Foo) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapFoo")

instance HasCField.HasCField Foo "unwrapFoo" where

  type CFieldType Foo "unwrapFoo" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/enum\/closed.h 3:3@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
pattern Bar :: Foo
pattern Bar = Foo 0

{-| __C declaration:__ @baz@

    __defined at:__ @binding-specs\/enum\/closed.h 4:3@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
pattern Baz :: Foo
pattern Baz = Foo 1

{-# COMPLETE Bar, Baz #-}

{-| __C declaration:__ @enum elementary_particle_type@

    __defined at:__ @binding-specs\/enum\/closed.h 8:6@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
newtype ElementaryParticleType = ElementaryParticleType
  { unwrapElementaryParticleType :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize ElementaryParticleType where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw ElementaryParticleType where

  readRaw =
    \ptr0 ->
          pure ElementaryParticleType
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw ElementaryParticleType where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          ElementaryParticleType unwrapElementaryParticleType2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapElementaryParticleType2

deriving via Marshal.EquivStorable ElementaryParticleType instance BG.Storable ElementaryParticleType

deriving via BG.CUInt instance BG.Prim ElementaryParticleType

instance CEnum.CEnum ElementaryParticleType where

  type CEnumZ ElementaryParticleType = BG.CUInt

  toCEnum = ElementaryParticleType

  fromCEnum =
    BG.getField @"unwrapElementaryParticleType"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [ (0, BG.singleton "Quark_up")
                                   , (1, BG.singleton "Quark_down")
                                   , (2, BG.singleton "Quark_charm")
                                   , (3, BG.singleton "Quark_strange")
                                   , (4, BG.singleton "Quark_top")
                                   , (5, BG.singleton "Quark_bottom")
                                   , (6, BG.singleton "Lepton_electron")
                                   , (7, BG.singleton "Lepton_electron_neutrino")
                                   , (8, BG.singleton "Lepton_muon")
                                   , (9, BG.singleton "Lepton_muon_neutrino")
                                   , (10, BG.singleton "Lepton_tau")
                                   , (11, BG.singleton "Lepton_tau_neutrino")
                                   , (12, BG.singleton "Boson_gluon")
                                   , (13, BG.singleton "Boson_photon")
                                   , (14, BG.singleton "Boson_z")
                                   , (15, BG.singleton "Boson_w")
                                   , (16, BG.singleton "Boson_higgs")
                                   ]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "ElementaryParticleType"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "ElementaryParticleType"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum ElementaryParticleType where

  minDeclaredValue = Quark_up

  maxDeclaredValue = Boson_higgs

instance Show ElementaryParticleType where

  showsPrec = CEnum.shows

instance Read ElementaryParticleType where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unwrapElementaryParticleType" ElementaryParticleType ty where

  hasField =
    \x0 ->
      ( \y1 ->
          ElementaryParticleType {unwrapElementaryParticleType = y1}
      , BG.getField @"unwrapElementaryParticleType" x0
      )

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapElementaryParticleType" (BG.Ptr ElementaryParticleType) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapElementaryParticleType")

instance HasCField.HasCField ElementaryParticleType "unwrapElementaryParticleType" where

  type CFieldType ElementaryParticleType "unwrapElementaryParticleType" =
    BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @quark_up@

    __defined at:__ @binding-specs\/enum\/closed.h 9:3@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
pattern Quark_up :: ElementaryParticleType
pattern Quark_up = ElementaryParticleType 0

{-| __C declaration:__ @quark_down@

    __defined at:__ @binding-specs\/enum\/closed.h 10:3@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
pattern Quark_down :: ElementaryParticleType
pattern Quark_down = ElementaryParticleType 1

{-| __C declaration:__ @quark_charm@

    __defined at:__ @binding-specs\/enum\/closed.h 11:3@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
pattern Quark_charm :: ElementaryParticleType
pattern Quark_charm = ElementaryParticleType 2

{-| __C declaration:__ @quark_strange@

    __defined at:__ @binding-specs\/enum\/closed.h 12:3@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
pattern Quark_strange :: ElementaryParticleType
pattern Quark_strange = ElementaryParticleType 3

{-| __C declaration:__ @quark_top@

    __defined at:__ @binding-specs\/enum\/closed.h 13:3@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
pattern Quark_top :: ElementaryParticleType
pattern Quark_top = ElementaryParticleType 4

{-| __C declaration:__ @quark_bottom@

    __defined at:__ @binding-specs\/enum\/closed.h 14:3@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
pattern Quark_bottom :: ElementaryParticleType
pattern Quark_bottom = ElementaryParticleType 5

{-| __C declaration:__ @lepton_electron@

    __defined at:__ @binding-specs\/enum\/closed.h 15:3@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
pattern Lepton_electron :: ElementaryParticleType
pattern Lepton_electron = ElementaryParticleType 6

{-| __C declaration:__ @lepton_electron_neutrino@

    __defined at:__ @binding-specs\/enum\/closed.h 16:3@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
pattern Lepton_electron_neutrino :: ElementaryParticleType
pattern Lepton_electron_neutrino = ElementaryParticleType 7

{-| __C declaration:__ @lepton_muon@

    __defined at:__ @binding-specs\/enum\/closed.h 17:3@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
pattern Lepton_muon :: ElementaryParticleType
pattern Lepton_muon = ElementaryParticleType 8

{-| __C declaration:__ @lepton_muon_neutrino@

    __defined at:__ @binding-specs\/enum\/closed.h 18:3@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
pattern Lepton_muon_neutrino :: ElementaryParticleType
pattern Lepton_muon_neutrino = ElementaryParticleType 9

{-| __C declaration:__ @lepton_tau@

    __defined at:__ @binding-specs\/enum\/closed.h 19:3@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
pattern Lepton_tau :: ElementaryParticleType
pattern Lepton_tau = ElementaryParticleType 10

{-| __C declaration:__ @lepton_tau_neutrino@

    __defined at:__ @binding-specs\/enum\/closed.h 20:3@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
pattern Lepton_tau_neutrino :: ElementaryParticleType
pattern Lepton_tau_neutrino = ElementaryParticleType 11

{-| __C declaration:__ @boson_gluon@

    __defined at:__ @binding-specs\/enum\/closed.h 21:3@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
pattern Boson_gluon :: ElementaryParticleType
pattern Boson_gluon = ElementaryParticleType 12

{-| __C declaration:__ @boson_photon@

    __defined at:__ @binding-specs\/enum\/closed.h 22:3@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
pattern Boson_photon :: ElementaryParticleType
pattern Boson_photon = ElementaryParticleType 13

{-| __C declaration:__ @boson_z@

    __defined at:__ @binding-specs\/enum\/closed.h 23:3@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
pattern Boson_z :: ElementaryParticleType
pattern Boson_z = ElementaryParticleType 14

{-| __C declaration:__ @boson_w@

    __defined at:__ @binding-specs\/enum\/closed.h 24:3@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
pattern Boson_w :: ElementaryParticleType
pattern Boson_w = ElementaryParticleType 15

{-| __C declaration:__ @boson_higgs@

    __defined at:__ @binding-specs\/enum\/closed.h 25:3@

    __exported by:__ @binding-specs\/enum\/closed.h@
-}
pattern Boson_higgs :: ElementaryParticleType
pattern Boson_higgs = ElementaryParticleType 16

{-# COMPLETE
      Quark_up
    , Quark_down
    , Quark_charm
    , Quark_strange
    , Quark_top
    , Quark_bottom
    , Lepton_electron
    , Lepton_electron_neutrino
    , Lepton_muon
    , Lepton_muon_neutrino
    , Lepton_tau
    , Lepton_tau_neutrino
    , Boson_gluon
    , Boson_photon
    , Boson_z
    , Boson_w
    , Boson_higgs
  #-}
