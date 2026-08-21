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
    ( Example.Hash_defines_buffer(..)
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct hash_defines_buffer@

    __defined at:__ @functions\/hash_defines.h 14:8@

    __exported by:__ @functions\/hash_defines.h@
-}
data Hash_defines_buffer = Hash_defines_buffer
  { hash_defines_buffer_data :: CA.ConstantArray 8 BG.CChar
    {- ^ __C declaration:__ @data@

         __defined at:__ @functions\/hash_defines.h 15:8@

         __exported by:__ @functions\/hash_defines.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Hash_defines_buffer where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Hash_defines_buffer where

  readRaw =
    \ptr0 ->
          pure Hash_defines_buffer
      <*> HasCField.readRaw (BG.Proxy @"hash_defines_buffer_data") ptr0

instance Marshal.WriteRaw Hash_defines_buffer where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Hash_defines_buffer hash_defines_buffer_data2 ->
            HasCField.writeRaw (BG.Proxy @"hash_defines_buffer_data") ptr0 hash_defines_buffer_data2

deriving via Marshal.EquivStorable Hash_defines_buffer instance BG.Storable Hash_defines_buffer

deriving via Struct.IsStructViaReadRaw Hash_defines_buffer instance Struct.IsStruct Hash_defines_buffer

{-| __C declaration:__ @data@

    __defined at:__ @functions\/hash_defines.h 15:8@

    __exported by:__ @functions\/hash_defines.h@
-}
instance ( ty ~ CA.ConstantArray 8 BG.CChar
         ) => BG.CompatHasField.HasField "hash_defines_buffer_data" Hash_defines_buffer ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Hash_defines_buffer {hash_defines_buffer_data = y1}
      , BG.getField @"hash_defines_buffer_data" x0
      )

instance ( ty ~ CA.ConstantArray 8 BG.CChar
         ) => BG.HasField "hash_defines_buffer_data" (BG.Ptr Hash_defines_buffer) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"hash_defines_buffer_data")

instance HasCField.HasCField Hash_defines_buffer "hash_defines_buffer_data" where

  type CFieldType Hash_defines_buffer "hash_defines_buffer_data" =
    CA.ConstantArray 8 BG.CChar

  offset# = \_ -> \_ -> 0
