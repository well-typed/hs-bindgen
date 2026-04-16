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
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.MyStruct(..)
    , Example.A(..)
    , Example.B(..)
    , Example.E(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified M

{-| __C declaration:__ @struct MyStruct@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 5:8@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
data MyStruct = MyStruct
  { myStruct_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 6:7@

         __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize MyStruct where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw MyStruct where

  readRaw =
    \ptr0 ->
          pure MyStruct
      <*> HasCField.readRaw (RIP.Proxy @"myStruct_x") ptr0

instance Marshal.WriteRaw MyStruct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          MyStruct myStruct_x2 ->
            HasCField.writeRaw (RIP.Proxy @"myStruct_x") ptr0 myStruct_x2

deriving via Marshal.EquivStorable MyStruct instance RIP.Storable MyStruct

instance HasCField.HasCField MyStruct "myStruct_x" where

  type CFieldType MyStruct "myStruct_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "myStruct_x" (RIP.Ptr MyStruct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"myStruct_x")

{-| __C declaration:__ @macro A@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 11:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
newtype A = A
  { unwrapA :: MyStruct
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) MyStruct
         ) => RIP.HasField "unwrapA" (RIP.Ptr A) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = MyStruct

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro B@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 12:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
newtype B = B
  { unwrapB :: A
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance (((~) ty) A) => RIP.HasField "unwrapB" (RIP.Ptr B) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapB")

instance HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro E@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 35:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
newtype E = E
  { unwrapE :: M.C
  }
  deriving stock (RIP.Generic)

instance (((~) ty) M.C) => RIP.HasField "unwrapE" (RIP.Ptr E) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapE")

instance HasCField.HasCField E "unwrapE" where

  type CFieldType E "unwrapE" = M.C

  offset# = \_ -> \_ -> 0
