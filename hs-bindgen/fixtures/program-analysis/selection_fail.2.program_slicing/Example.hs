{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct OkBefore@

    __defined at:__ @program-analysis\/selection_fail.h 1:8@

    __exported by:__ @program-analysis\/selection_fail.h@
-}
data OkBefore = OkBefore
  { okBefore_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @program-analysis\/selection_fail.h 2:7@

         __exported by:__ @program-analysis\/selection_fail.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize OkBefore where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw OkBefore where

  readRaw =
    \ptr0 ->
          pure OkBefore
      <*> HasCField.readRaw (RIP.Proxy @"okBefore_x") ptr0

instance Marshal.WriteRaw OkBefore where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          OkBefore okBefore_x2 ->
            HasCField.writeRaw (RIP.Proxy @"okBefore_x") ptr0 okBefore_x2

deriving via Marshal.EquivStorable OkBefore instance RIP.Storable OkBefore

instance HasCField.HasCField OkBefore "okBefore_x" where

  type CFieldType OkBefore "okBefore_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "okBefore_x" (RIP.Ptr OkBefore) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"okBefore_x")

{-| __C declaration:__ @struct OkAfter@

    __defined at:__ @program-analysis\/selection_fail.h 26:8@

    __exported by:__ @program-analysis\/selection_fail.h@
-}
data OkAfter = OkAfter
  { okAfter_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @program-analysis\/selection_fail.h 27:7@

         __exported by:__ @program-analysis\/selection_fail.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize OkAfter where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw OkAfter where

  readRaw =
    \ptr0 ->
          pure OkAfter
      <*> HasCField.readRaw (RIP.Proxy @"okAfter_x") ptr0

instance Marshal.WriteRaw OkAfter where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          OkAfter okAfter_x2 ->
            HasCField.writeRaw (RIP.Proxy @"okAfter_x") ptr0 okAfter_x2

deriving via Marshal.EquivStorable OkAfter instance RIP.Storable OkAfter

instance HasCField.HasCField OkAfter "okAfter_x" where

  type CFieldType OkAfter "okAfter_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "okAfter_x" (RIP.Ptr OkAfter) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"okAfter_x")
