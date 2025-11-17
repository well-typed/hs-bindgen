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

{-| __defined at:__ @types\/anonymous.h:3:3@

    __exported by:__ @types\/anonymous.h@
-}
data S1_c = S1_c
  { s1_c_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/anonymous.h:4:9@

         __exported by:__ @types\/anonymous.h@
    -}
  , s1_c_b :: FC.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/anonymous.h:5:9@

         __exported by:__ @types\/anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S1_c where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S1_c
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s1_c_a") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s1_c_b") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1_c s1_c_a2 s1_c_b3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s1_c_a") ptr0 s1_c_a2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s1_c_b") ptr0 s1_c_b3

instance HsBindgen.Runtime.HasCField.HasCField S1_c "s1_c_a" where

  type CFieldType S1_c "s1_c_a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S1_c) "s1_c_a")
         ) => GHC.Records.HasField "s1_c_a" (Ptr.Ptr S1_c) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s1_c_a")

instance HsBindgen.Runtime.HasCField.HasCField S1_c "s1_c_b" where

  type CFieldType S1_c "s1_c_b" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S1_c) "s1_c_b")
         ) => GHC.Records.HasField "s1_c_b" (Ptr.Ptr S1_c) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s1_c_b")

{-| __C declaration:__ @S1@

    __defined at:__ @types\/anonymous.h:2:8@

    __exported by:__ @types\/anonymous.h@
-}
data S1 = S1
  { s1_c :: S1_c
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/anonymous.h:6:5@

         __exported by:__ @types\/anonymous.h@
    -}
  , s1_d :: FC.CInt
    {- ^ __C declaration:__ @d@

         __defined at:__ @types\/anonymous.h:8:7@

         __exported by:__ @types\/anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S1 where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S1
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s1_c") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s1_d") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1 s1_c2 s1_d3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s1_c") ptr0 s1_c2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s1_d") ptr0 s1_d3

instance HsBindgen.Runtime.HasCField.HasCField S1 "s1_c" where

  type CFieldType S1 "s1_c" = S1_c

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S1) "s1_c")
         ) => GHC.Records.HasField "s1_c" (Ptr.Ptr S1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s1_c")

instance HsBindgen.Runtime.HasCField.HasCField S1 "s1_d" where

  type CFieldType S1 "s1_d" = FC.CInt

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S1) "s1_d")
         ) => GHC.Records.HasField "s1_d" (Ptr.Ptr S1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s1_d")

{-| __defined at:__ @types\/anonymous.h:15:5@

    __exported by:__ @types\/anonymous.h@
-}
data S2_inner_deep = S2_inner_deep
  { s2_inner_deep_b :: FC.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/anonymous.h:16:11@

         __exported by:__ @types\/anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S2_inner_deep where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S2_inner_deep
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s2_inner_deep_b") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_inner_deep s2_inner_deep_b2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s2_inner_deep_b") ptr0 s2_inner_deep_b2

instance HsBindgen.Runtime.HasCField.HasCField S2_inner_deep "s2_inner_deep_b" where

  type CFieldType S2_inner_deep "s2_inner_deep_b" =
    FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S2_inner_deep) "s2_inner_deep_b")
         ) => GHC.Records.HasField "s2_inner_deep_b" (Ptr.Ptr S2_inner_deep) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s2_inner_deep_b")

{-| __defined at:__ @types\/anonymous.h:13:3@

    __exported by:__ @types\/anonymous.h@
-}
data S2_inner = S2_inner
  { s2_inner_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/anonymous.h:14:9@

         __exported by:__ @types\/anonymous.h@
    -}
  , s2_inner_deep :: S2_inner_deep
    {- ^ __C declaration:__ @deep@

         __defined at:__ @types\/anonymous.h:17:7@

         __exported by:__ @types\/anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S2_inner where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S2_inner
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s2_inner_a") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s2_inner_deep") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_inner s2_inner_a2 s2_inner_deep3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s2_inner_a") ptr0 s2_inner_a2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s2_inner_deep") ptr0 s2_inner_deep3

instance HsBindgen.Runtime.HasCField.HasCField S2_inner "s2_inner_a" where

  type CFieldType S2_inner "s2_inner_a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S2_inner) "s2_inner_a")
         ) => GHC.Records.HasField "s2_inner_a" (Ptr.Ptr S2_inner) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s2_inner_a")

instance HsBindgen.Runtime.HasCField.HasCField S2_inner "s2_inner_deep" where

  type CFieldType S2_inner "s2_inner_deep" =
    S2_inner_deep

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S2_inner) "s2_inner_deep")
         ) => GHC.Records.HasField "s2_inner_deep" (Ptr.Ptr S2_inner) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s2_inner_deep")

{-| __C declaration:__ @S2@

    __defined at:__ @types\/anonymous.h:12:8@

    __exported by:__ @types\/anonymous.h@
-}
data S2 = S2
  { s2_inner :: S2_inner
    {- ^ __C declaration:__ @inner@

         __defined at:__ @types\/anonymous.h:18:5@

         __exported by:__ @types\/anonymous.h@
    -}
  , s2_d :: FC.CInt
    {- ^ __C declaration:__ @d@

         __defined at:__ @types\/anonymous.h:20:7@

         __exported by:__ @types\/anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S2 where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S2
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s2_inner") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s2_d") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2 s2_inner2 s2_d3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s2_inner") ptr0 s2_inner2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s2_d") ptr0 s2_d3

instance HsBindgen.Runtime.HasCField.HasCField S2 "s2_inner" where

  type CFieldType S2 "s2_inner" = S2_inner

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S2) "s2_inner")
         ) => GHC.Records.HasField "s2_inner" (Ptr.Ptr S2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s2_inner")

instance HsBindgen.Runtime.HasCField.HasCField S2 "s2_d" where

  type CFieldType S2 "s2_d" = FC.CInt

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S2) "s2_d")
         ) => GHC.Records.HasField "s2_d" (Ptr.Ptr S2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s2_d")

{-| __C declaration:__ @S3@

    __defined at:__ @types\/anonymous.h:24:8@

    __exported by:__ @types\/anonymous.h@
-}
data S3 = S3
  { s3_c :: Ptr.Ptr (Ptr.Ptr S3_c)
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/anonymous.h:28:7@

         __exported by:__ @types\/anonymous.h@
    -}
  , s3_d :: FC.CInt
    {- ^ __C declaration:__ @d@

         __defined at:__ @types\/anonymous.h:30:7@

         __exported by:__ @types\/anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S3 where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure S3
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s3_c") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s3_d") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S3 s3_c2 s3_d3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s3_c") ptr0 s3_c2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s3_d") ptr0 s3_d3

instance HsBindgen.Runtime.HasCField.HasCField S3 "s3_c" where

  type CFieldType S3 "s3_c" = Ptr.Ptr (Ptr.Ptr S3_c)

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S3) "s3_c")
         ) => GHC.Records.HasField "s3_c" (Ptr.Ptr S3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s3_c")

instance HsBindgen.Runtime.HasCField.HasCField S3 "s3_d" where

  type CFieldType S3 "s3_d" = FC.CInt

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S3) "s3_d")
         ) => GHC.Records.HasField "s3_d" (Ptr.Ptr S3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s3_d")

{-| __defined at:__ @types\/anonymous.h:25:3@

    __exported by:__ @types\/anonymous.h@
-}
data S3_c = S3_c
  { s3_c_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/anonymous.h:26:9@

         __exported by:__ @types\/anonymous.h@
    -}
  , s3_c_b :: FC.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/anonymous.h:27:9@

         __exported by:__ @types\/anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S3_c where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S3_c
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s3_c_a") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s3_c_b") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S3_c s3_c_a2 s3_c_b3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s3_c_a") ptr0 s3_c_a2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s3_c_b") ptr0 s3_c_b3

instance HsBindgen.Runtime.HasCField.HasCField S3_c "s3_c_a" where

  type CFieldType S3_c "s3_c_a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S3_c) "s3_c_a")
         ) => GHC.Records.HasField "s3_c_a" (Ptr.Ptr S3_c) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s3_c_a")

instance HsBindgen.Runtime.HasCField.HasCField S3_c "s3_c_b" where

  type CFieldType S3_c "s3_c_b" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S3_c) "s3_c_b")
         ) => GHC.Records.HasField "s3_c_b" (Ptr.Ptr S3_c) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s3_c_b")
