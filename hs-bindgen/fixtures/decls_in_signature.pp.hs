{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Data.Array.Byte
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.SizedByteArray
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure)

$(CAPI.addCSource "#include \"decls_in_signature.h\"\nvoid testmodule_normal (struct opaque *arg1, struct outside *arg2, struct outside *arg3) { normal(arg1, arg2, *arg3); }\nvoid testmodule_f1 (struct named_struct *arg1) { f1(*arg1); }\nvoid testmodule_f2 (union named_union *arg1) { f2(*arg1); }\n")

data Opaque

data Outside = Outside
  { outside_x :: FC.CInt
  , outside_y :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable Outside where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Outside
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Outside outside_x2 outside_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) outside_x2
            >> F.pokeByteOff ptr0 (4 :: Int) outside_y3

foreign import ccall safe "testmodule_normal" normal_wrapper :: (F.Ptr Opaque) -> (F.Ptr Outside) -> (F.Ptr Outside) -> IO ()

normal :: (F.Ptr Opaque) -> (F.Ptr Outside) -> Outside -> IO ()
normal = \x0 -> \x1 -> \x2 -> F.with x2 (\y3 -> normal_wrapper x0 x1 y3)

data Named_struct = Named_struct
  { named_struct_x :: FC.CInt
  , named_struct_y :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable Named_struct where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Named_struct
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Named_struct named_struct_x2 named_struct_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) named_struct_x2
            >> F.pokeByteOff ptr0 (4 :: Int) named_struct_y3

foreign import ccall safe "testmodule_f1" f1_wrapper :: (F.Ptr Named_struct) -> IO ()

f1 :: Named_struct -> IO ()
f1 = \x0 -> F.with x0 (\y1 -> f1_wrapper y1)

newtype Named_union = Named_union
  { un_Named_union :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable Named_union

get_named_union_x :: Named_union -> FC.CInt
get_named_union_x = HsBindgen.Runtime.ByteArray.getUnionPayload

set_named_union_x :: FC.CInt -> Named_union
set_named_union_x = HsBindgen.Runtime.ByteArray.setUnionPayload

get_named_union_y :: Named_union -> FC.CChar
get_named_union_y = HsBindgen.Runtime.ByteArray.getUnionPayload

set_named_union_y :: FC.CChar -> Named_union
set_named_union_y = HsBindgen.Runtime.ByteArray.setUnionPayload

foreign import ccall safe "testmodule_f2" f2_wrapper :: (F.Ptr Named_union) -> IO ()

f2 :: Named_union -> IO ()
f2 = \x0 -> F.with x0 (\y1 -> f2_wrapper y1)
