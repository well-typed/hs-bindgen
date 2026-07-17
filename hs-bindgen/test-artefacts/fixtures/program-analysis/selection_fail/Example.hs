{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.OkBefore(..)
    , Example.OkAfter(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct OkBefore@

    __defined at:__ @program-analysis\/selection_fail.h 1:8@

    __exported by:__ @program-analysis\/selection_fail.h@
-}
data OkBefore = OkBefore
  { okBefore_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @program-analysis\/selection_fail.h 2:7@

         __exported by:__ @program-analysis\/selection_fail.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize OkBefore where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw OkBefore where

  readRaw =
    \ptr0 ->
          pure OkBefore
      <*> HasCField.readRaw (BG.Proxy @"okBefore_x") ptr0

instance Marshal.WriteRaw OkBefore where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          OkBefore okBefore_x2 ->
            HasCField.writeRaw (BG.Proxy @"okBefore_x") ptr0 okBefore_x2

deriving via Marshal.EquivStorable OkBefore instance BG.Storable OkBefore

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "okBefore_x" OkBefore ty where

  hasField =
    \x0 ->
      (\y1 ->
         OkBefore {okBefore_x = y1}, BG.getField @"okBefore_x" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "okBefore_x" (BG.Ptr OkBefore) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"okBefore_x")

instance HasCField.HasCField OkBefore "okBefore_x" where

  type CFieldType OkBefore "okBefore_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct OkAfter@

    __defined at:__ @program-analysis\/selection_fail.h 26:8@

    __exported by:__ @program-analysis\/selection_fail.h@
-}
data OkAfter = OkAfter
  { okAfter_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @program-analysis\/selection_fail.h 27:7@

         __exported by:__ @program-analysis\/selection_fail.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize OkAfter where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw OkAfter where

  readRaw =
    \ptr0 ->
          pure OkAfter
      <*> HasCField.readRaw (BG.Proxy @"okAfter_x") ptr0

instance Marshal.WriteRaw OkAfter where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          OkAfter okAfter_x2 ->
            HasCField.writeRaw (BG.Proxy @"okAfter_x") ptr0 okAfter_x2

deriving via Marshal.EquivStorable OkAfter instance BG.Storable OkAfter

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "okAfter_x" OkAfter ty where

  hasField =
    \x0 ->
      (\y1 ->
         OkAfter {okAfter_x = y1}, BG.getField @"okAfter_x" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "okAfter_x" (BG.Ptr OkAfter) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"okAfter_x")

instance HasCField.HasCField OkAfter "okAfter_x" where

  type CFieldType OkAfter "okAfter_x" = BG.CInt

  offset# = \_ -> \_ -> 0
