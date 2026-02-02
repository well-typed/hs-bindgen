{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (Eq, Show)

{-| __C declaration:__ @S@

    __defined at:__ @arrays\/const_qualifier.h 7:19@

    __exported by:__ @arrays\/const_qualifier.h@
-}
newtype S = S
  { unwrapS :: HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt
  }
  deriving stock (Eq, Show)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S) "unwrapS")
         ) => GHC.Records.HasField "unwrapS" (Ptr.Ptr S) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapS")

instance HsBindgen.Runtime.HasCField.HasCField S "unwrapS" where

  type CFieldType S "unwrapS" =
    HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T@

    __defined at:__ @arrays\/const_qualifier.h 8:19@

    __exported by:__ @arrays\/const_qualifier.h@
-}
newtype T = T
  { unwrapT :: HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt
  }
  deriving stock (Eq, Show)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType T) "unwrapT")
         ) => GHC.Records.HasField "unwrapT" (Ptr.Ptr T) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapT")

instance HsBindgen.Runtime.HasCField.HasCField T "unwrapT" where

  type CFieldType T "unwrapT" =
    HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @U@

    __defined at:__ @arrays\/const_qualifier.h 18:19@

    __exported by:__ @arrays\/const_qualifier.h@
-}
newtype U = U
  { unwrapU :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }
  deriving stock (Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType U) "unwrapU")
         ) => GHC.Records.HasField "unwrapU" (Ptr.Ptr U) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapU")

instance HsBindgen.Runtime.HasCField.HasCField U "unwrapU" where

  type CFieldType U "unwrapU" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @V@

    __defined at:__ @arrays\/const_qualifier.h 19:19@

    __exported by:__ @arrays\/const_qualifier.h@
-}
newtype V = V
  { unwrapV :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }
  deriving stock (Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType V) "unwrapV")
         ) => GHC.Records.HasField "unwrapV" (Ptr.Ptr V) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapV")

instance HsBindgen.Runtime.HasCField.HasCField V "unwrapV" where

  type CFieldType V "unwrapV" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @W@

    __defined at:__ @arrays\/const_qualifier.h 29:19@

    __exported by:__ @arrays\/const_qualifier.h@
-}
newtype W = W
  { unwrapW :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }
  deriving stock (Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType W) "unwrapW")
         ) => GHC.Records.HasField "unwrapW" (Ptr.Ptr W) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapW")

instance HsBindgen.Runtime.HasCField.HasCField W "unwrapW" where

  type CFieldType W "unwrapW" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @X@

    __defined at:__ @arrays\/const_qualifier.h 30:19@

    __exported by:__ @arrays\/const_qualifier.h@
-}
newtype X = X
  { unwrapX :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }
  deriving stock (Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType X) "unwrapX")
         ) => GHC.Records.HasField "unwrapX" (Ptr.Ptr X) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapX")

instance HsBindgen.Runtime.HasCField.HasCField X "unwrapX" where

  type CFieldType X "unwrapX" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt

  offset# = \_ -> \_ -> 0
