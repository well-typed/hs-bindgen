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
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.My_int_t(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @my_int_t@

    __defined at:__ @macros\/reparse\/functions.h 19:16@

    __exported by:__ @macros\/reparse\/functions.h@
-}
newtype My_int_t = My_int_t
  { unwrapMy_int_t :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapMy_int_t" My_int_t ty where

  hasField =
    \x0 ->
      (\y1 ->
         My_int_t {unwrapMy_int_t = y1}, BG.getField @"unwrapMy_int_t" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapMy_int_t" (BG.Ptr My_int_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMy_int_t")

instance HasCField.HasCField My_int_t "unwrapMy_int_t" where

  type CFieldType My_int_t "unwrapMy_int_t" = BG.CInt

  offset# = \_ -> \_ -> 0
