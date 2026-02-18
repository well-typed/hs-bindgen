{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @int_t@

    __defined at:__ @declarations\/redeclaration.h 20:13@

    __exported by:__ @declarations\/redeclaration.h@
-}
newtype Int_t = Int_t
  { unwrapInt_t :: RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.Bitfield
    , RIP.Bits
    , Bounded
    , Enum
    , RIP.FiniteBits
    , RIP.HasFFIType
    , Integral
    , RIP.Ix
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unwrapInt_t" (RIP.Ptr Int_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapInt_t")

instance HasCField.HasCField Int_t "unwrapInt_t" where

  type CFieldType Int_t "unwrapInt_t" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct X@

    __defined at:__ @declarations\/redeclaration.h 26:8@

    __exported by:__ @declarations\/redeclaration.h@
-}
data X = X
  { x_n :: RIP.CInt
    {- ^ __C declaration:__ @n@

         __defined at:__ @declarations\/redeclaration.h 26:16@

         __exported by:__ @declarations\/redeclaration.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize X where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw X where

  readRaw =
    \ptr0 ->
          pure X
      <*> HasCField.readRaw (RIP.Proxy @"x_n") ptr0

instance Marshal.WriteRaw X where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          X x_n2 ->
            HasCField.writeRaw (RIP.Proxy @"x_n") ptr0 x_n2

deriving via Marshal.EquivStorable X instance RIP.Storable X

instance HasCField.HasCField X "x_n" where

  type CFieldType X "x_n" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "x_n" (RIP.Ptr X) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"x_n")

{-| __C declaration:__ @union Y@

    __defined at:__ @declarations\/redeclaration.h 30:7@

    __exported by:__ @declarations\/redeclaration.h@
-}
newtype Y = Y
  { unwrapY :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize Y

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw Y

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw Y

deriving via Marshal.EquivStorable Y instance RIP.Storable Y

{-|

  __See:__ 'set_y_m'

__C declaration:__ @m@

__defined at:__ @declarations\/redeclaration.h 30:15@

__exported by:__ @declarations\/redeclaration.h@
-}
get_y_m ::
     Y
  -> RIP.CInt
get_y_m = RIP.getUnionPayload

{-|

  __See:__ 'get_y_m'

-}
set_y_m ::
     RIP.CInt
  -> Y
set_y_m = RIP.setUnionPayload

{-|

  __See:__ 'set_y_o'

__C declaration:__ @o@

__defined at:__ @declarations\/redeclaration.h 30:22@

__exported by:__ @declarations\/redeclaration.h@
-}
get_y_o ::
     Y
  -> RIP.CInt
get_y_o = RIP.getUnionPayload

{-|

  __See:__ 'get_y_o'

-}
set_y_o ::
     RIP.CInt
  -> Y
set_y_o = RIP.setUnionPayload

instance HasCField.HasCField Y "y_m" where

  type CFieldType Y "y_m" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "y_m" (RIP.Ptr Y) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"y_m")

instance HasCField.HasCField Y "y_o" where

  type CFieldType Y "y_o" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "y_o" (RIP.Ptr Y) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"y_o")
