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

module MyLib.Mylib.Internal
    ( MyLib.Mylib.Internal.Internal_state_t(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct internal_state_t@

    __defined at:__ @mylib\/internal.h 4:9@

    __exported by:__ @mylib.h@
-}
data Internal_state_t = Internal_state_t
  { internal_state_t_refcount :: BG.CInt
    {- ^ __C declaration:__ @refcount@

         __defined at:__ @mylib\/internal.h 4:22@

         __exported by:__ @mylib.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Internal_state_t where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Internal_state_t where

  readRaw =
    \ptr0 ->
          pure Internal_state_t
      <*> HasCField.readRaw (BG.Proxy @"internal_state_t_refcount") ptr0

instance Marshal.WriteRaw Internal_state_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Internal_state_t internal_state_t_refcount2 ->
            HasCField.writeRaw (BG.Proxy @"internal_state_t_refcount") ptr0 internal_state_t_refcount2

deriving via Marshal.EquivStorable Internal_state_t instance BG.Storable Internal_state_t

deriving via Struct.IsStructViaReadRaw Internal_state_t instance Struct.IsStruct Internal_state_t

{-| __C declaration:__ @refcount@

    __defined at:__ @mylib\/internal.h 4:22@

    __exported by:__ @mylib.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "internal_state_t_refcount" Internal_state_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Internal_state_t {internal_state_t_refcount = y1}
      , BG.getField @"internal_state_t_refcount" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "internal_state_t_refcount" (BG.Ptr Internal_state_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"internal_state_t_refcount")

instance HasCField.HasCField Internal_state_t "internal_state_t_refcount" where

  type CFieldType Internal_state_t "internal_state_t_refcount" =
    BG.CInt

  offset# = \_ -> \_ -> 0
