{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.S(..)
    , Example.T(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct s@

    __defined at:__ @program-analysis\/selection_squash_into_selected.h 3:8@

    __exported by:__ @program-analysis\/selection_squash_into_selected.h@
-}
data S = S
  { s_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @program-analysis\/selection_squash_into_selected.h 4:7@

         __exported by:__ @program-analysis\/selection_squash_into_selected.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S where

  readRaw =
    \ptr0 ->
          pure S
      <*> HasCField.readRaw (BG.Proxy @"s_x") ptr0

instance Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_x2 ->
            HasCField.writeRaw (BG.Proxy @"s_x") ptr0 s_x2

deriving via Marshal.EquivStorable S instance BG.Storable S

deriving via Struct.IsStructViaReadRaw S instance Struct.IsStruct S

{-| __C declaration:__ @x@

    __defined at:__ @program-analysis\/selection_squash_into_selected.h 4:7@

    __exported by:__ @program-analysis\/selection_squash_into_selected.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "s_x" S ty where

  hasField =
    \x0 -> (\y1 -> S {s_x = y1}, BG.getField @"s_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "s_x" (BG.Ptr S) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s_x")

instance HasCField.HasCField S "s_x" where

  type CFieldType S "s_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct t@

    __defined at:__ @program-analysis\/selection_squash_into_selected.h 9:8@

    __exported by:__ @program-analysis\/selection_squash_into_selected.h@
-}
data T = T
  { t_ptr :: BG.Ptr S
    {- ^ __C declaration:__ @ptr@

         __defined at:__ @program-analysis\/selection_squash_into_selected.h 10:6@

         __exported by:__ @program-analysis\/selection_squash_into_selected.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize T where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw T where

  readRaw =
    \ptr0 ->
          pure T
      <*> HasCField.readRaw (BG.Proxy @"t_ptr") ptr0

instance Marshal.WriteRaw T where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T t_ptr2 ->
            HasCField.writeRaw (BG.Proxy @"t_ptr") ptr0 t_ptr2

deriving via Marshal.EquivStorable T instance BG.Storable T

deriving via Struct.IsStructViaReadRaw T instance Struct.IsStruct T

{-| __C declaration:__ @ptr@

    __defined at:__ @program-analysis\/selection_squash_into_selected.h 10:6@

    __exported by:__ @program-analysis\/selection_squash_into_selected.h@
-}
instance (ty ~ BG.Ptr S) => BG.CompatHasField.HasField "t_ptr" T ty where

  hasField =
    \x0 ->
      (\y1 -> T {t_ptr = y1}, BG.getField @"t_ptr" x0)

instance (ty ~ BG.Ptr S) => BG.HasField "t_ptr" (BG.Ptr T) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t_ptr")

instance HasCField.HasCField T "t_ptr" where

  type CFieldType T "t_ptr" = BG.Ptr S

  offset# = \_ -> \_ -> 0
