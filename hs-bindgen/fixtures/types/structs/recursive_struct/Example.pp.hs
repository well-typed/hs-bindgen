{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @linked_list_A_t@

    __defined at:__ @types\/structs\/recursive_struct.h:1:16@

    __exported by:__ @types\/structs\/recursive_struct.h@
-}
data Linked_list_A_t = Linked_list_A_t
  { linked_list_A_t_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/recursive_struct.h:2:7@

         __exported by:__ @types\/structs\/recursive_struct.h@
    -}
  , linked_list_A_t_next :: Ptr.Ptr Linked_list_A_t
    {- ^ __C declaration:__ @next@

         __defined at:__ @types\/structs\/recursive_struct.h:3:27@

         __exported by:__ @types\/structs\/recursive_struct.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Linked_list_A_t where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Linked_list_A_t
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"linked_list_A_t_x") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"linked_list_A_t_next") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Linked_list_A_t linked_list_A_t_x2 linked_list_A_t_next3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"linked_list_A_t_x") ptr0 linked_list_A_t_x2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"linked_list_A_t_next") ptr0 linked_list_A_t_next3

instance HsBindgen.Runtime.HasCField.HasCField Linked_list_A_t "linked_list_A_t_x" where

  type CFieldType Linked_list_A_t "linked_list_A_t_x" =
    FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Linked_list_A_t) "linked_list_A_t_x")
         ) => GHC.Records.HasField "linked_list_A_t_x" (Ptr.Ptr Linked_list_A_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"linked_list_A_t_x")

instance HsBindgen.Runtime.HasCField.HasCField Linked_list_A_t "linked_list_A_t_next" where

  type CFieldType Linked_list_A_t "linked_list_A_t_next" =
    Ptr.Ptr Linked_list_A_t

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Linked_list_A_t) "linked_list_A_t_next")
         ) => GHC.Records.HasField "linked_list_A_t_next" (Ptr.Ptr Linked_list_A_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"linked_list_A_t_next")

{-| __C declaration:__ @linked_list_B_t@

    __defined at:__ @types\/structs\/recursive_struct.h:9:8@

    __exported by:__ @types\/structs\/recursive_struct.h@
-}
data Linked_list_B_t = Linked_list_B_t
  { linked_list_B_t_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/recursive_struct.h:10:7@

         __exported by:__ @types\/structs\/recursive_struct.h@
    -}
  , linked_list_B_t_next :: Ptr.Ptr Linked_list_B_t
    {- ^ __C declaration:__ @next@

         __defined at:__ @types\/structs\/recursive_struct.h:11:20@

         __exported by:__ @types\/structs\/recursive_struct.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Linked_list_B_t where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Linked_list_B_t
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"linked_list_B_t_x") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"linked_list_B_t_next") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Linked_list_B_t linked_list_B_t_x2 linked_list_B_t_next3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"linked_list_B_t_x") ptr0 linked_list_B_t_x2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"linked_list_B_t_next") ptr0 linked_list_B_t_next3

instance HsBindgen.Runtime.HasCField.HasCField Linked_list_B_t "linked_list_B_t_x" where

  type CFieldType Linked_list_B_t "linked_list_B_t_x" =
    FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Linked_list_B_t) "linked_list_B_t_x")
         ) => GHC.Records.HasField "linked_list_B_t_x" (Ptr.Ptr Linked_list_B_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"linked_list_B_t_x")

instance HsBindgen.Runtime.HasCField.HasCField Linked_list_B_t "linked_list_B_t_next" where

  type CFieldType Linked_list_B_t "linked_list_B_t_next" =
    Ptr.Ptr Linked_list_B_t

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Linked_list_B_t) "linked_list_B_t_next")
         ) => GHC.Records.HasField "linked_list_B_t_next" (Ptr.Ptr Linked_list_B_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"linked_list_B_t_next")
