{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.FLAM as FLAM
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Pascal_Aux

instance Eq Pascal_Aux

instance BG.Generic Pascal_Aux

instance Show Pascal_Aux

instance Marshal.StaticSize Pascal_Aux

instance Marshal.ReadRaw Pascal_Aux

instance Marshal.WriteRaw Pascal_Aux

instance BG.Storable Pascal_Aux

instance Struct.IsStruct Pascal_Aux

type Pascal = FLAM.WithFlam BG.CChar Pascal_Aux

data Foo_bar

instance Eq Foo_bar

instance BG.Generic Foo_bar

instance Show Foo_bar

instance Marshal.StaticSize Foo_bar

instance Marshal.ReadRaw Foo_bar

instance Marshal.WriteRaw Foo_bar

instance BG.Storable Foo_bar

instance Struct.IsStruct Foo_bar

data Foo_Aux

instance Eq Foo_Aux

instance BG.Generic Foo_Aux

instance Show Foo_Aux

instance Marshal.StaticSize Foo_Aux

instance Marshal.ReadRaw Foo_Aux

instance Marshal.WriteRaw Foo_Aux

instance BG.Storable Foo_Aux

instance Struct.IsStruct Foo_Aux

type Foo = FLAM.WithFlam Foo_bar Foo_Aux

data Diff_Aux

instance Eq Diff_Aux

instance BG.Generic Diff_Aux

instance Show Diff_Aux

instance Marshal.StaticSize Diff_Aux

instance Marshal.ReadRaw Diff_Aux

instance Marshal.WriteRaw Diff_Aux

instance BG.Storable Diff_Aux

instance Struct.IsStruct Diff_Aux

type Diff = FLAM.WithFlam BG.CChar Diff_Aux

data Triplets_Aux

instance Eq Triplets_Aux

instance BG.Generic Triplets_Aux

instance Show Triplets_Aux

instance Marshal.StaticSize Triplets_Aux

instance Marshal.ReadRaw Triplets_Aux

instance Marshal.WriteRaw Triplets_Aux

instance BG.Storable Triplets_Aux

instance Struct.IsStruct Triplets_Aux

type Triplets =
  FLAM.WithFlam (CA.ConstantArray 3 BG.CInt) Triplets_Aux
