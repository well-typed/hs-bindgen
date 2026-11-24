{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Small(..)
    , pattern Example.SMALL_A
    , pattern Example.SMALL_B
    , pattern Example.BYTE_A
    , pattern Example.BYTE_B
    , pattern Example.SHORT_A
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, type (~))

{-| __C declaration:__ @small@

    __defined at:__ @types\/enums\/untagged_enum_fixed_type.h 10:23@

    __exported by:__ @types\/enums\/untagged_enum_fixed_type.h@
-}
newtype Small = Small
  { unwrapSmall :: BG.CUChar
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

instance ( ty ~ BG.CUChar
         ) => BG.CompatHasField.HasField "unwrapSmall" Small ty where

  hasField =
    \x0 ->
      (\y1 ->
         Small {unwrapSmall = y1}, BG.getField @"unwrapSmall" x0)

instance ( ty ~ BG.CUChar
         ) => BG.HasField "unwrapSmall" (BG.Ptr Small) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapSmall")

instance HasCField.HasCField Small "unwrapSmall" where

  type CFieldType Small "unwrapSmall" = BG.CUChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @SMALL_A@

    __defined at:__ @types\/enums\/untagged_enum_fixed_type.h 13:1@

    __exported by:__ @types\/enums\/untagged_enum_fixed_type.h@
-}
pattern SMALL_A :: Small
pattern SMALL_A = 0

{-| __C declaration:__ @SMALL_B@

    __defined at:__ @types\/enums\/untagged_enum_fixed_type.h 13:1@

    __exported by:__ @types\/enums\/untagged_enum_fixed_type.h@
-}
pattern SMALL_B :: Small
pattern SMALL_B = 1

{-| __C declaration:__ @BYTE_A@

    __defined at:__ @types\/enums\/untagged_enum_fixed_type.h 19:1@

    __exported by:__ @types\/enums\/untagged_enum_fixed_type.h@
-}
pattern BYTE_A :: HsBindgen.Runtime.LibC.Word8
pattern BYTE_A = 0

{-| __C declaration:__ @BYTE_B@

    __defined at:__ @types\/enums\/untagged_enum_fixed_type.h 19:1@

    __exported by:__ @types\/enums\/untagged_enum_fixed_type.h@
-}
pattern BYTE_B :: HsBindgen.Runtime.LibC.Word8
pattern BYTE_B = 255

{-| __C declaration:__ @SHORT_A@

    __defined at:__ @types\/enums\/untagged_enum_fixed_type.h 25:1@

    __exported by:__ @types\/enums\/untagged_enum_fixed_type.h@
-}
pattern SHORT_A :: BG.CUShort
pattern SHORT_A = 0
