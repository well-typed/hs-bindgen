{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @S@

    __defined at:__ @arrays\/const_qualifier.h 7:19@

    __exported by:__ @arrays\/const_qualifier.h@
-}
newtype S = S
  { unwrapS :: IA.IncompleteArray RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Show)

instance ( ((~) ty) (IA.IncompleteArray RIP.CInt)
         ) => RIP.HasField "unwrapS" (RIP.Ptr S) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapS")

instance HasCField.HasCField S "unwrapS" where

  type CFieldType S "unwrapS" =
    IA.IncompleteArray RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T@

    __defined at:__ @arrays\/const_qualifier.h 8:19@

    __exported by:__ @arrays\/const_qualifier.h@
-}
newtype T = T
  { unwrapT :: IA.IncompleteArray RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Show)

instance ( ((~) ty) (IA.IncompleteArray RIP.CInt)
         ) => RIP.HasField "unwrapT" (RIP.Ptr T) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapT")

instance HasCField.HasCField T "unwrapT" where

  type CFieldType T "unwrapT" =
    IA.IncompleteArray RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @U@

    __defined at:__ @arrays\/const_qualifier.h 18:19@

    __exported by:__ @arrays\/const_qualifier.h@
-}
newtype U = U
  { unwrapU :: (CA.ConstantArray 3) RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) ((CA.ConstantArray 3) RIP.CInt)
         ) => RIP.HasField "unwrapU" (RIP.Ptr U) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapU")

instance HasCField.HasCField U "unwrapU" where

  type CFieldType U "unwrapU" =
    (CA.ConstantArray 3) RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @V@

    __defined at:__ @arrays\/const_qualifier.h 19:19@

    __exported by:__ @arrays\/const_qualifier.h@
-}
newtype V = V
  { unwrapV :: (CA.ConstantArray 3) RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) ((CA.ConstantArray 3) RIP.CInt)
         ) => RIP.HasField "unwrapV" (RIP.Ptr V) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapV")

instance HasCField.HasCField V "unwrapV" where

  type CFieldType V "unwrapV" =
    (CA.ConstantArray 3) RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @W@

    __defined at:__ @arrays\/const_qualifier.h 29:19@

    __exported by:__ @arrays\/const_qualifier.h@
-}
newtype W = W
  { unwrapW :: (CA.ConstantArray 3) RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) ((CA.ConstantArray 3) RIP.CInt)
         ) => RIP.HasField "unwrapW" (RIP.Ptr W) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapW")

instance HasCField.HasCField W "unwrapW" where

  type CFieldType W "unwrapW" =
    (CA.ConstantArray 3) RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @X@

    __defined at:__ @arrays\/const_qualifier.h 30:19@

    __exported by:__ @arrays\/const_qualifier.h@
-}
newtype X = X
  { unwrapX :: (CA.ConstantArray 3) RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) ((CA.ConstantArray 3) RIP.CInt)
         ) => RIP.HasField "unwrapX" (RIP.Ptr X) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapX")

instance HasCField.HasCField X "unwrapX" where

  type CFieldType X "unwrapX" =
    (CA.ConstantArray 3) RIP.CInt

  offset# = \_ -> \_ -> 0
