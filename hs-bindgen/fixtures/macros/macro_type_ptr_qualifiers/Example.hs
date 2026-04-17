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
    ( Example.PtrToVoid(..)
    , Example.PtrToConstVoidL(..)
    , Example.PtrToConstVoidR(..)
    , Example.PtrToConstIntL(..)
    , Example.PtrToConstIntR(..)
    , Example.ConstPtrToInt(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.PtrConst as PtrConst

{-| __C declaration:__ @macro PtrToVoid@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 2:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
newtype PtrToVoid = PtrToVoid
  { unwrapPtrToVoid :: RIP.Ptr RIP.Void
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( (~) ty (RIP.Ptr RIP.Void)
         ) => RIP.HasField "unwrapPtrToVoid" (RIP.Ptr PtrToVoid) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPtrToVoid")

instance HasCField.HasCField PtrToVoid "unwrapPtrToVoid" where

  type CFieldType PtrToVoid "unwrapPtrToVoid" =
    RIP.Ptr RIP.Void

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro PtrToConstVoidL@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 5:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
newtype PtrToConstVoidL = PtrToConstVoidL
  { unwrapPtrToConstVoidL :: PtrConst.PtrConst RIP.Void
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( (~) ty (PtrConst.PtrConst RIP.Void)
         ) => RIP.HasField "unwrapPtrToConstVoidL" (RIP.Ptr PtrToConstVoidL) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPtrToConstVoidL")

instance HasCField.HasCField PtrToConstVoidL "unwrapPtrToConstVoidL" where

  type CFieldType PtrToConstVoidL "unwrapPtrToConstVoidL" =
    PtrConst.PtrConst RIP.Void

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro PtrToConstVoidR@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 8:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
newtype PtrToConstVoidR = PtrToConstVoidR
  { unwrapPtrToConstVoidR :: PtrConst.PtrConst RIP.Void
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( (~) ty (PtrConst.PtrConst RIP.Void)
         ) => RIP.HasField "unwrapPtrToConstVoidR" (RIP.Ptr PtrToConstVoidR) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPtrToConstVoidR")

instance HasCField.HasCField PtrToConstVoidR "unwrapPtrToConstVoidR" where

  type CFieldType PtrToConstVoidR "unwrapPtrToConstVoidR" =
    PtrConst.PtrConst RIP.Void

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro PtrToConstIntL@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 11:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
newtype PtrToConstIntL = PtrToConstIntL
  { unwrapPtrToConstIntL :: PtrConst.PtrConst RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( (~) ty (PtrConst.PtrConst RIP.CInt)
         ) => RIP.HasField "unwrapPtrToConstIntL" (RIP.Ptr PtrToConstIntL) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPtrToConstIntL")

instance HasCField.HasCField PtrToConstIntL "unwrapPtrToConstIntL" where

  type CFieldType PtrToConstIntL "unwrapPtrToConstIntL" =
    PtrConst.PtrConst RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro PtrToConstIntR@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 14:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
newtype PtrToConstIntR = PtrToConstIntR
  { unwrapPtrToConstIntR :: PtrConst.PtrConst RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( (~) ty (PtrConst.PtrConst RIP.CInt)
         ) => RIP.HasField "unwrapPtrToConstIntR" (RIP.Ptr PtrToConstIntR) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPtrToConstIntR")

instance HasCField.HasCField PtrToConstIntR "unwrapPtrToConstIntR" where

  type CFieldType PtrToConstIntR "unwrapPtrToConstIntR" =
    PtrConst.PtrConst RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro ConstPtrToInt@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 17:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
newtype ConstPtrToInt = ConstPtrToInt
  { unwrapConstPtrToInt :: RIP.Ptr RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( (~) ty (RIP.Ptr RIP.CInt)
         ) => RIP.HasField "unwrapConstPtrToInt" (RIP.Ptr ConstPtrToInt) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConstPtrToInt")

instance HasCField.HasCField ConstPtrToInt "unwrapConstPtrToInt" where

  type CFieldType ConstPtrToInt "unwrapConstPtrToInt" =
    RIP.Ptr RIP.CInt

  offset# = \_ -> \_ -> 0
