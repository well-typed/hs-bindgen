{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import qualified M
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct MyStruct@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/struct.h 4:8@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/struct.h@
-}
data MyStruct = MyStruct
  { myStruct_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @binding-specs\/fun_arg\/typedef\/struct.h 4:23@

         __exported by:__ @binding-specs\/fun_arg\/typedef\/struct.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize MyStruct where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw MyStruct where

  readRaw =
    \ptr0 ->
          pure MyStruct
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"myStruct_x") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw MyStruct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          MyStruct myStruct_x2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"myStruct_x") ptr0 myStruct_x2

deriving via HsBindgen.Runtime.Marshal.EquivStorable MyStruct instance F.Storable MyStruct

instance HsBindgen.Runtime.HasCField.HasCField MyStruct "myStruct_x" where

  type CFieldType MyStruct "myStruct_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "myStruct_x" (Ptr.Ptr MyStruct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"myStruct_x")

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/struct.h 7:25@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/struct.h@
-}
newtype A = A
  { unwrapA :: MyStruct
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance ( TyEq ty MyStruct
         ) => GHC.Records.HasField "unwrapA" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapA")

instance HsBindgen.Runtime.HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = MyStruct

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/struct.h 8:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/struct.h@
-}
newtype B = B
  { unwrapB :: A
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance ( TyEq ty A
         ) => GHC.Records.HasField "unwrapB" (Ptr.Ptr B) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapB")

instance HsBindgen.Runtime.HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @E@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/struct.h 21:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/struct.h@
-}
newtype E = E
  { unwrapE :: M.C
  }
  deriving stock (GHC.Generics.Generic)

instance ( TyEq ty M.C
         ) => GHC.Records.HasField "unwrapE" (Ptr.Ptr E) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapE")

instance HsBindgen.Runtime.HasCField.HasCField E "unwrapE" where

  type CFieldType E "unwrapE" = M.C

  offset# = \_ -> \_ -> 0
