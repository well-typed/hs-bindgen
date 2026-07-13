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
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @macro PtrToVoid@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 2:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
newtype PtrToVoid = PtrToVoid
  { unwrapPtrToVoid :: BG.Ptr BG.Void
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr BG.Void
         ) => BG.CompatHasField.HasField "unwrapPtrToVoid" PtrToVoid ty where

  hasField =
    \x0 ->
      (\y1 ->
         PtrToVoid {unwrapPtrToVoid = y1}, BG.getField @"unwrapPtrToVoid" x0)

instance ( ty ~ BG.Ptr BG.Void
         ) => BG.HasField "unwrapPtrToVoid" (BG.Ptr PtrToVoid) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPtrToVoid")

instance HasCField.HasCField PtrToVoid "unwrapPtrToVoid" where

  type CFieldType PtrToVoid "unwrapPtrToVoid" =
    BG.Ptr BG.Void

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro PtrToConstVoidL@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 5:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
newtype PtrToConstVoidL = PtrToConstVoidL
  { unwrapPtrToConstVoidL :: PtrConst.PtrConst BG.Void
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ PtrConst.PtrConst BG.Void
         ) => BG.CompatHasField.HasField "unwrapPtrToConstVoidL" PtrToConstVoidL ty where

  hasField =
    \x0 ->
      ( \y1 -> PtrToConstVoidL {unwrapPtrToConstVoidL = y1}
      , BG.getField @"unwrapPtrToConstVoidL" x0
      )

instance ( ty ~ PtrConst.PtrConst BG.Void
         ) => BG.HasField "unwrapPtrToConstVoidL" (BG.Ptr PtrToConstVoidL) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPtrToConstVoidL")

instance HasCField.HasCField PtrToConstVoidL "unwrapPtrToConstVoidL" where

  type CFieldType PtrToConstVoidL "unwrapPtrToConstVoidL" =
    PtrConst.PtrConst BG.Void

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro PtrToConstVoidR@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 8:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
newtype PtrToConstVoidR = PtrToConstVoidR
  { unwrapPtrToConstVoidR :: PtrConst.PtrConst BG.Void
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ PtrConst.PtrConst BG.Void
         ) => BG.CompatHasField.HasField "unwrapPtrToConstVoidR" PtrToConstVoidR ty where

  hasField =
    \x0 ->
      ( \y1 -> PtrToConstVoidR {unwrapPtrToConstVoidR = y1}
      , BG.getField @"unwrapPtrToConstVoidR" x0
      )

instance ( ty ~ PtrConst.PtrConst BG.Void
         ) => BG.HasField "unwrapPtrToConstVoidR" (BG.Ptr PtrToConstVoidR) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPtrToConstVoidR")

instance HasCField.HasCField PtrToConstVoidR "unwrapPtrToConstVoidR" where

  type CFieldType PtrToConstVoidR "unwrapPtrToConstVoidR" =
    PtrConst.PtrConst BG.Void

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro PtrToConstIntL@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 11:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
newtype PtrToConstIntL = PtrToConstIntL
  { unwrapPtrToConstIntL :: PtrConst.PtrConst BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.CompatHasField.HasField "unwrapPtrToConstIntL" PtrToConstIntL ty where

  hasField =
    \x0 ->
      ( \y1 -> PtrToConstIntL {unwrapPtrToConstIntL = y1}
      , BG.getField @"unwrapPtrToConstIntL" x0
      )

instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.HasField "unwrapPtrToConstIntL" (BG.Ptr PtrToConstIntL) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPtrToConstIntL")

instance HasCField.HasCField PtrToConstIntL "unwrapPtrToConstIntL" where

  type CFieldType PtrToConstIntL "unwrapPtrToConstIntL" =
    PtrConst.PtrConst BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro PtrToConstIntR@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 14:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
newtype PtrToConstIntR = PtrToConstIntR
  { unwrapPtrToConstIntR :: PtrConst.PtrConst BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.CompatHasField.HasField "unwrapPtrToConstIntR" PtrToConstIntR ty where

  hasField =
    \x0 ->
      ( \y1 -> PtrToConstIntR {unwrapPtrToConstIntR = y1}
      , BG.getField @"unwrapPtrToConstIntR" x0
      )

instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.HasField "unwrapPtrToConstIntR" (BG.Ptr PtrToConstIntR) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPtrToConstIntR")

instance HasCField.HasCField PtrToConstIntR "unwrapPtrToConstIntR" where

  type CFieldType PtrToConstIntR "unwrapPtrToConstIntR" =
    PtrConst.PtrConst BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro ConstPtrToInt@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 17:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
newtype ConstPtrToInt = ConstPtrToInt
  { unwrapConstPtrToInt :: BG.Ptr BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.CompatHasField.HasField "unwrapConstPtrToInt" ConstPtrToInt ty where

  hasField =
    \x0 ->
      ( \y1 -> ConstPtrToInt {unwrapConstPtrToInt = y1}
      , BG.getField @"unwrapConstPtrToInt" x0
      )

instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.HasField "unwrapConstPtrToInt" (BG.Ptr ConstPtrToInt) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConstPtrToInt")

instance HasCField.HasCField ConstPtrToInt "unwrapConstPtrToInt" where

  type CFieldType ConstPtrToInt "unwrapConstPtrToInt" =
    BG.Ptr BG.CInt

  offset# = \_ -> \_ -> 0
