{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.iD
    , Example.MY_INT(..)
    , Example.My_int_t(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @macro ID@

    __defined at:__ @macros\/reparse\/functions.h 1:9@

    __exported by:__ @macros\/reparse\/functions.h@
-}
iD :: forall a0. a0 -> a0
iD = \x0 -> x0

{-| __C declaration:__ @macro MY_INT@

    __defined at:__ @macros\/reparse\/functions.h 2:9@

    __exported by:__ @macros\/reparse\/functions.h@
-}
newtype MY_INT = MY_INT
  { unwrapMY_INT :: BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "unwrapMY_INT" MY_INT ty where

  hasField =
    \x0 ->
      (\y1 ->
         MY_INT {unwrapMY_INT = y1}, BG.getField @"unwrapMY_INT" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapMY_INT" (BG.Ptr MY_INT) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMY_INT")

instance HasCField.HasCField MY_INT "unwrapMY_INT" where

  type CFieldType MY_INT "unwrapMY_INT" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @my_int_t@

    __defined at:__ @macros\/reparse\/functions.h 19:16@

    __exported by:__ @macros\/reparse\/functions.h@
-}
newtype My_int_t = My_int_t
  { unwrapMy_int_t :: MY_INT
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ MY_INT
         ) => BG.CompatHasField.HasField "unwrapMy_int_t" My_int_t ty where

  hasField =
    \x0 ->
      (\y1 ->
         My_int_t {unwrapMy_int_t = y1}, BG.getField @"unwrapMy_int_t" x0)

instance ( ty ~ MY_INT
         ) => BG.HasField "unwrapMy_int_t" (BG.Ptr My_int_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMy_int_t")

instance HasCField.HasCField My_int_t "unwrapMy_int_t" where

  type CFieldType My_int_t "unwrapMy_int_t" = MY_INT

  offset# = \_ -> \_ -> 0
