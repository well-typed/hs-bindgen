{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.fun
    , Example.Unsafe.param_underscore
    , Example.Unsafe.param_uppercase
    , Example.Unsafe.param_undersore_capital
    , Example.Unsafe.param_haskell_reserved_name
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <functions/heap_types/struct.h>"
  , "void hs_bindgen_c4af6bb824712c6a ("
  , "  T *arg1,"
  , "  T *arg2"
  , ")"
  , "{"
  , "  *arg2 = (fun)(*arg1);"
  , "}"
  , "void hs_bindgen_80a83be02e40bf03 ("
  , "  T *arg1"
  , ")"
  , "{"
  , "  (param_underscore)(*arg1);"
  , "}"
  , "void hs_bindgen_bda30535ff7fd9f4 ("
  , "  T *arg1"
  , ")"
  , "{"
  , "  (param_uppercase)(*arg1);"
  , "}"
  , "void hs_bindgen_7b5f5cbdd8a5ff0e ("
  , "  T *arg1"
  , ")"
  , "{"
  , "  (param_undersore_capital)(*arg1);"
  , "}"
  , "void hs_bindgen_5b71629b3ccc3fa8 ("
  , "  T *arg1"
  , ")"
  , "{"
  , "  (param_haskell_reserved_name)(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsheap_typesstruct_Example_Unsafe_fun@
foreign import ccall unsafe "hs_bindgen_c4af6bb824712c6a" hs_bindgen_c4af6bb824712c6a_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_functionsheap_typesstruct_Example_Unsafe_fun@
hs_bindgen_c4af6bb824712c6a ::
     RIP.Ptr T
  -> RIP.Ptr T
  -> IO ()
hs_bindgen_c4af6bb824712c6a =
  RIP.fromFFIType hs_bindgen_c4af6bb824712c6a_base

{-| __C declaration:__ @fun@

    __defined at:__ @functions\/heap_types\/struct.h 9:3@

    __exported by:__ @functions\/heap_types\/struct.h@
-}
fun ::
     T
     -- ^ __C declaration:__ @x@
  -> IO T
fun =
  \x0 ->
    RIP.with x0 (\x1 ->
                   RIP.allocaAndPeek (\res2 ->
                                        hs_bindgen_c4af6bb824712c6a x1 res2))

-- __unique:__ @test_functionsheap_typesstruct_Example_Unsafe_param_underscore@
foreign import ccall unsafe "hs_bindgen_80a83be02e40bf03" hs_bindgen_80a83be02e40bf03_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_functionsheap_typesstruct_Example_Unsafe_param_underscore@
hs_bindgen_80a83be02e40bf03 ::
     RIP.Ptr T
  -> IO ()
hs_bindgen_80a83be02e40bf03 =
  RIP.fromFFIType hs_bindgen_80a83be02e40bf03_base

{-| __C declaration:__ @param_underscore@

    __defined at:__ @functions\/heap_types\/struct.h 12:6@

    __exported by:__ @functions\/heap_types\/struct.h@
-}
param_underscore ::
     T
     -- ^ __C declaration:__ @_@
  -> IO ()
param_underscore =
  \_0 ->
    RIP.with _0 (\_1 -> hs_bindgen_80a83be02e40bf03 _1)

-- __unique:__ @test_functionsheap_typesstruct_Example_Unsafe_param_uppercase@
foreign import ccall unsafe "hs_bindgen_bda30535ff7fd9f4" hs_bindgen_bda30535ff7fd9f4_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_functionsheap_typesstruct_Example_Unsafe_param_uppercase@
hs_bindgen_bda30535ff7fd9f4 ::
     RIP.Ptr T
  -> IO ()
hs_bindgen_bda30535ff7fd9f4 =
  RIP.fromFFIType hs_bindgen_bda30535ff7fd9f4_base

{-| __C declaration:__ @param_uppercase@

    __defined at:__ @functions\/heap_types\/struct.h 13:6@

    __exported by:__ @functions\/heap_types\/struct.h@
-}
param_uppercase ::
     T
     -- ^ __C declaration:__ @Type@
  -> IO ()
param_uppercase =
  \type'0 ->
    RIP.with type'0 (\type'1 ->
                       hs_bindgen_bda30535ff7fd9f4 type'1)

-- __unique:__ @test_functionsheap_typesstruct_Example_Unsafe_param_undersore_capital@
foreign import ccall unsafe "hs_bindgen_7b5f5cbdd8a5ff0e" hs_bindgen_7b5f5cbdd8a5ff0e_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_functionsheap_typesstruct_Example_Unsafe_param_undersore_capital@
hs_bindgen_7b5f5cbdd8a5ff0e ::
     RIP.Ptr T
  -> IO ()
hs_bindgen_7b5f5cbdd8a5ff0e =
  RIP.fromFFIType hs_bindgen_7b5f5cbdd8a5ff0e_base

{-| __C declaration:__ @param_undersore_capital@

    __defined at:__ @functions\/heap_types\/struct.h 14:6@

    __exported by:__ @functions\/heap_types\/struct.h@
-}
param_undersore_capital ::
     T
     -- ^ __C declaration:__ @_T@
  -> IO ()
param_undersore_capital =
  \_T0 ->
    RIP.with _T0 (\_T1 ->
                    hs_bindgen_7b5f5cbdd8a5ff0e _T1)

-- __unique:__ @test_functionsheap_typesstruct_Example_Unsafe_param_haskell_reserved_name@
foreign import ccall unsafe "hs_bindgen_5b71629b3ccc3fa8" hs_bindgen_5b71629b3ccc3fa8_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_functionsheap_typesstruct_Example_Unsafe_param_haskell_reserved_name@
hs_bindgen_5b71629b3ccc3fa8 ::
     RIP.Ptr T
  -> IO ()
hs_bindgen_5b71629b3ccc3fa8 =
  RIP.fromFFIType hs_bindgen_5b71629b3ccc3fa8_base

{-| __C declaration:__ @param_haskell_reserved_name@

    __defined at:__ @functions\/heap_types\/struct.h 15:6@

    __exported by:__ @functions\/heap_types\/struct.h@
-}
param_haskell_reserved_name ::
     T
     -- ^ __C declaration:__ @type@
  -> IO ()
param_haskell_reserved_name =
  \type'0 ->
    RIP.with type'0 (\type'1 ->
                       hs_bindgen_5b71629b3ccc3fa8 type'1)
