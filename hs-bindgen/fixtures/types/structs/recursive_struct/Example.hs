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

{-| __C declaration:__ @struct linked_list_A_s@

    __defined at:__ @types\/structs\/recursive_struct.h 1:16@

    __exported by:__ @types\/structs\/recursive_struct.h@
-}
data Linked_list_A_t = Linked_list_A_t
  { linked_list_A_t_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/recursive_struct.h 2:7@

         __exported by:__ @types\/structs\/recursive_struct.h@
    -}
  , linked_list_A_t_next :: RIP.Ptr Linked_list_A_t
    {- ^ __C declaration:__ @next@

         __defined at:__ @types\/structs\/recursive_struct.h 3:27@

         __exported by:__ @types\/structs\/recursive_struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Linked_list_A_t where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Linked_list_A_t where

  readRaw =
    \ptr0 ->
          pure Linked_list_A_t
      <*> HasCField.readRaw (RIP.Proxy @"linked_list_A_t_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"linked_list_A_t_next") ptr0

instance Marshal.WriteRaw Linked_list_A_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Linked_list_A_t linked_list_A_t_x2 linked_list_A_t_next3 ->
               HasCField.writeRaw (RIP.Proxy @"linked_list_A_t_x") ptr0 linked_list_A_t_x2
            >> HasCField.writeRaw (RIP.Proxy @"linked_list_A_t_next") ptr0 linked_list_A_t_next3

deriving via Marshal.EquivStorable Linked_list_A_t instance RIP.Storable Linked_list_A_t

instance HasCField.HasCField Linked_list_A_t "linked_list_A_t_x" where

  type CFieldType Linked_list_A_t "linked_list_A_t_x" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "linked_list_A_t_x" (RIP.Ptr Linked_list_A_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"linked_list_A_t_x")

instance HasCField.HasCField Linked_list_A_t "linked_list_A_t_next" where

  type CFieldType Linked_list_A_t "linked_list_A_t_next" =
    RIP.Ptr Linked_list_A_t

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) (RIP.Ptr Linked_list_A_t)
         ) => RIP.HasField "linked_list_A_t_next" (RIP.Ptr Linked_list_A_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"linked_list_A_t_next")

{-| __C declaration:__ @struct linked_list_B_t@

    __defined at:__ @types\/structs\/recursive_struct.h 9:8@

    __exported by:__ @types\/structs\/recursive_struct.h@
-}
data Linked_list_B_t = Linked_list_B_t
  { linked_list_B_t_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/recursive_struct.h 10:7@

         __exported by:__ @types\/structs\/recursive_struct.h@
    -}
  , linked_list_B_t_next :: RIP.Ptr Linked_list_B_t
    {- ^ __C declaration:__ @next@

         __defined at:__ @types\/structs\/recursive_struct.h 11:20@

         __exported by:__ @types\/structs\/recursive_struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Linked_list_B_t where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Linked_list_B_t where

  readRaw =
    \ptr0 ->
          pure Linked_list_B_t
      <*> HasCField.readRaw (RIP.Proxy @"linked_list_B_t_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"linked_list_B_t_next") ptr0

instance Marshal.WriteRaw Linked_list_B_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Linked_list_B_t linked_list_B_t_x2 linked_list_B_t_next3 ->
               HasCField.writeRaw (RIP.Proxy @"linked_list_B_t_x") ptr0 linked_list_B_t_x2
            >> HasCField.writeRaw (RIP.Proxy @"linked_list_B_t_next") ptr0 linked_list_B_t_next3

deriving via Marshal.EquivStorable Linked_list_B_t instance RIP.Storable Linked_list_B_t

instance HasCField.HasCField Linked_list_B_t "linked_list_B_t_x" where

  type CFieldType Linked_list_B_t "linked_list_B_t_x" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "linked_list_B_t_x" (RIP.Ptr Linked_list_B_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"linked_list_B_t_x")

instance HasCField.HasCField Linked_list_B_t "linked_list_B_t_next" where

  type CFieldType Linked_list_B_t "linked_list_B_t_next" =
    RIP.Ptr Linked_list_B_t

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) (RIP.Ptr Linked_list_B_t)
         ) => RIP.HasField "linked_list_B_t_next" (RIP.Ptr Linked_list_B_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"linked_list_B_t_next")
