{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/structs/struct_arg_const.h>"
  , "/* test_typesstructsstruct_arg_const_Example_get_fun_const_arg */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_444a89a1a48acd31 (void)) ("
  , "  struct thing const arg1"
  , ")"
  , "{"
  , "  return &fun_const_arg;"
  , "}"
  , "/* test_typesstructsstruct_arg_const_Example_get_fun_const_result */"
  , "__attribute__ ((const))"
  , "struct thing const (*hs_bindgen_3840e50b04084db6 (void)) (void)"
  , "{"
  , "  return &fun_const_result;"
  , "}"
  , "/* test_typesstructsstruct_arg_const_Example_get_fun_const */"
  , "__attribute__ ((const))"
  , "struct thing const (*hs_bindgen_49a195a2b05d425e (void)) ("
  , "  struct thing const arg1"
  , ")"
  , "{"
  , "  return &fun_const;"
  , "}"
  ]))

-- __unique:__ @test_typesstructsstruct_arg_const_Example_get_fun_const_arg@
foreign import ccall unsafe "hs_bindgen_444a89a1a48acd31" hs_bindgen_444a89a1a48acd31 ::
     IO (Ptr.FunPtr (Thing -> IO ()))

{-# NOINLINE fun_const_arg #-}
{-| __C declaration:__ @fun_const_arg@

    __defined at:__ @types\/structs\/struct_arg_const.h:7:20@

    __exported by:__ @types\/structs\/struct_arg_const.h@
-}
fun_const_arg :: Ptr.FunPtr (Thing -> IO ())
fun_const_arg =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_444a89a1a48acd31

-- __unique:__ @test_typesstructsstruct_arg_const_Example_get_fun_const_result@
foreign import ccall unsafe "hs_bindgen_3840e50b04084db6" hs_bindgen_3840e50b04084db6 ::
     IO (Ptr.FunPtr (IO Thing))

{-# NOINLINE fun_const_result #-}
{-| __C declaration:__ @fun_const_result@

    __defined at:__ @types\/structs\/struct_arg_const.h:8:20@

    __exported by:__ @types\/structs\/struct_arg_const.h@
-}
fun_const_result :: Ptr.FunPtr (IO Thing)
fun_const_result =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3840e50b04084db6

-- __unique:__ @test_typesstructsstruct_arg_const_Example_get_fun_const@
foreign import ccall unsafe "hs_bindgen_49a195a2b05d425e" hs_bindgen_49a195a2b05d425e ::
     IO (Ptr.FunPtr (Thing -> IO Thing))

{-# NOINLINE fun_const #-}
{-| __C declaration:__ @fun_const@

    __defined at:__ @types\/structs\/struct_arg_const.h:9:20@

    __exported by:__ @types\/structs\/struct_arg_const.h@
-}
fun_const :: Ptr.FunPtr (Thing -> IO Thing)
fun_const =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_49a195a2b05d425e
