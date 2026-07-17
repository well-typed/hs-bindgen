{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.S(..)
    , Example.T(..)
    , Example.U(..)
    , Example.V(..)
    , Example.W(..)
    , Example.X(..)
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @S@

    __defined at:__ @arrays\/const_qualifier.h 7:19@

    __exported by:__ @arrays\/const_qualifier.h@
-}
newtype S = S
  { unwrapS :: IA.IncompleteArray BG.CInt
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype (IsA.IsArray)

instance ( ty ~ IA.IncompleteArray BG.CInt
         ) => BG.CompatHasField.HasField "unwrapS" S ty where

  hasField =
    \x0 ->
      (\y1 -> S {unwrapS = y1}, BG.getField @"unwrapS" x0)

instance ( ty ~ IA.IncompleteArray BG.CInt
         ) => BG.HasField "unwrapS" (BG.Ptr S) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapS")

instance HasCField.HasCField S "unwrapS" where

  type CFieldType S "unwrapS" =
    IA.IncompleteArray BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T@

    __defined at:__ @arrays\/const_qualifier.h 8:19@

    __exported by:__ @arrays\/const_qualifier.h@
-}
newtype T = T
  { unwrapT :: IA.IncompleteArray BG.CInt
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype (IsA.IsArray)

instance ( ty ~ IA.IncompleteArray BG.CInt
         ) => BG.CompatHasField.HasField "unwrapT" T ty where

  hasField =
    \x0 ->
      (\y1 -> T {unwrapT = y1}, BG.getField @"unwrapT" x0)

instance ( ty ~ IA.IncompleteArray BG.CInt
         ) => BG.HasField "unwrapT" (BG.Ptr T) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT")

instance HasCField.HasCField T "unwrapT" where

  type CFieldType T "unwrapT" =
    IA.IncompleteArray BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @U@

    __defined at:__ @arrays\/const_qualifier.h 18:19@

    __exported by:__ @arrays\/const_qualifier.h@
-}
newtype U = U
  { unwrapU :: CA.ConstantArray 3 BG.CInt
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ CA.ConstantArray 3 BG.CInt
         ) => BG.CompatHasField.HasField "unwrapU" U ty where

  hasField =
    \x0 ->
      (\y1 -> U {unwrapU = y1}, BG.getField @"unwrapU" x0)

instance ( ty ~ CA.ConstantArray 3 BG.CInt
         ) => BG.HasField "unwrapU" (BG.Ptr U) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapU")

instance HasCField.HasCField U "unwrapU" where

  type CFieldType U "unwrapU" =
    CA.ConstantArray 3 BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @V@

    __defined at:__ @arrays\/const_qualifier.h 19:19@

    __exported by:__ @arrays\/const_qualifier.h@
-}
newtype V = V
  { unwrapV :: CA.ConstantArray 3 BG.CInt
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ CA.ConstantArray 3 BG.CInt
         ) => BG.CompatHasField.HasField "unwrapV" V ty where

  hasField =
    \x0 ->
      (\y1 -> V {unwrapV = y1}, BG.getField @"unwrapV" x0)

instance ( ty ~ CA.ConstantArray 3 BG.CInt
         ) => BG.HasField "unwrapV" (BG.Ptr V) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapV")

instance HasCField.HasCField V "unwrapV" where

  type CFieldType V "unwrapV" =
    CA.ConstantArray 3 BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @W@

    __defined at:__ @arrays\/const_qualifier.h 29:19@

    __exported by:__ @arrays\/const_qualifier.h@
-}
newtype W = W
  { unwrapW :: CA.ConstantArray 3 BG.CInt
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ CA.ConstantArray 3 BG.CInt
         ) => BG.CompatHasField.HasField "unwrapW" W ty where

  hasField =
    \x0 ->
      (\y1 -> W {unwrapW = y1}, BG.getField @"unwrapW" x0)

instance ( ty ~ CA.ConstantArray 3 BG.CInt
         ) => BG.HasField "unwrapW" (BG.Ptr W) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapW")

instance HasCField.HasCField W "unwrapW" where

  type CFieldType W "unwrapW" =
    CA.ConstantArray 3 BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @X@

    __defined at:__ @arrays\/const_qualifier.h 30:19@

    __exported by:__ @arrays\/const_qualifier.h@
-}
newtype X = X
  { unwrapX :: CA.ConstantArray 3 BG.CInt
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ CA.ConstantArray 3 BG.CInt
         ) => BG.CompatHasField.HasField "unwrapX" X ty where

  hasField =
    \x0 ->
      (\y1 -> X {unwrapX = y1}, BG.getField @"unwrapX" x0)

instance ( ty ~ CA.ConstantArray 3 BG.CInt
         ) => BG.HasField "unwrapX" (BG.Ptr X) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapX")

instance HasCField.HasCField X "unwrapX" where

  type CFieldType X "unwrapX" =
    CA.ConstantArray 3 BG.CInt

  offset# = \_ -> \_ -> 0
