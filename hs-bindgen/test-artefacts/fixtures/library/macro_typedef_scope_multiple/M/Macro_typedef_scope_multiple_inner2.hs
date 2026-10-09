{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module M.Macro_typedef_scope_multiple_inner2
    ( M.Macro_typedef_scope_multiple_inner2.M3(..)
    , M.Macro_typedef_scope_multiple_inner2.T4(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified M.Macro_typedef_scope_multiple_inner1
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, type (~))

{-| __C declaration:__ @macro M3@

    __defined at:__ @macro_typedef_scope_multiple_inner2.h 1:9@

    __exported by:__ @golden\/macros\/parse\/macro_typedef_scope_multiple.h@
-}
newtype M3 = M3
  { unwrapM3 :: M.Macro_typedef_scope_multiple_inner1.T2
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

instance ( ty ~ M.Macro_typedef_scope_multiple_inner1.T2
         ) => BG.CompatHasField.HasField "unwrapM3" M3 ty where

  hasField =
    \x0 ->
      (\y1 ->
         M3 {unwrapM3 = y1}, BG.getField @"unwrapM3" x0)

instance ( ty ~ M.Macro_typedef_scope_multiple_inner1.T2
         ) => BG.HasField "unwrapM3" (BG.Ptr M3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapM3")

instance HasCField.HasCField M3 "unwrapM3" where

  type CFieldType M3 "unwrapM3" =
    M.Macro_typedef_scope_multiple_inner1.T2

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T4@

    __defined at:__ @macro_typedef_scope_multiple_inner2.h 2:12@

    __exported by:__ @golden\/macros\/parse\/macro_typedef_scope_multiple.h@
-}
newtype T4 = T4
  { unwrapT4 :: M3
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

instance (ty ~ M3) => BG.CompatHasField.HasField "unwrapT4" T4 ty where

  hasField =
    \x0 ->
      (\y1 ->
         T4 {unwrapT4 = y1}, BG.getField @"unwrapT4" x0)

instance (ty ~ M3) => BG.HasField "unwrapT4" (BG.Ptr T4) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT4")

instance HasCField.HasCField T4 "unwrapT4" where

  type CFieldType T4 "unwrapT4" = M3

  offset# = \_ -> \_ -> 0
