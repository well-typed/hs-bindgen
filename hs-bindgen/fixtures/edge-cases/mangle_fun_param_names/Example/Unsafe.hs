{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.param_underscore
    , Example.Unsafe.param_uppercase
    , Example.Unsafe.param_undersore_capital
    , Example.Unsafe.param_haskell_reserved_name
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <edge-cases/mangle_fun_param_names.h>"
  , "void hs_bindgen_0ff5c586f093fdab ("
  , "  T *arg1"
  , ")"
  , "{"
  , "  (param_underscore)(*arg1);"
  , "}"
  , "void hs_bindgen_83f7b80a01e50f06 ("
  , "  T *arg1"
  , ")"
  , "{"
  , "  (param_uppercase)(*arg1);"
  , "}"
  , "void hs_bindgen_a015d9b3e63c0a07 ("
  , "  T *arg1"
  , ")"
  , "{"
  , "  (param_undersore_capital)(*arg1);"
  , "}"
  , "void hs_bindgen_6ac28bcdfd5bcd07 ("
  , "  T *arg1"
  , ")"
  , "{"
  , "  (param_haskell_reserved_name)(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_Unsafe_param_underscore@
foreign import ccall unsafe "hs_bindgen_0ff5c586f093fdab" hs_bindgen_0ff5c586f093fdab_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_Unsafe_param_underscore@
hs_bindgen_0ff5c586f093fdab ::
     RIP.Ptr T
  -> IO ()
hs_bindgen_0ff5c586f093fdab =
  RIP.fromFFIType hs_bindgen_0ff5c586f093fdab_base

{-| __C declaration:__ @param_underscore@

    __defined at:__ @edge-cases\/mangle_fun_param_names.h 9:6@

    __exported by:__ @edge-cases\/mangle_fun_param_names.h@
-}
param_underscore ::
     T
     -- ^ __C declaration:__ @_@
  -> IO ()
param_underscore =
  \_0 ->
    RIP.with _0 (\_1 -> hs_bindgen_0ff5c586f093fdab _1)

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_Unsafe_param_uppercase@
foreign import ccall unsafe "hs_bindgen_83f7b80a01e50f06" hs_bindgen_83f7b80a01e50f06_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_Unsafe_param_uppercase@
hs_bindgen_83f7b80a01e50f06 ::
     RIP.Ptr T
  -> IO ()
hs_bindgen_83f7b80a01e50f06 =
  RIP.fromFFIType hs_bindgen_83f7b80a01e50f06_base

{-| __C declaration:__ @param_uppercase@

    __defined at:__ @edge-cases\/mangle_fun_param_names.h 10:6@

    __exported by:__ @edge-cases\/mangle_fun_param_names.h@
-}
param_uppercase ::
     T
     -- ^ __C declaration:__ @Type@
  -> IO ()
param_uppercase =
  \type'0 ->
    RIP.with type'0 (\type'1 ->
                       hs_bindgen_83f7b80a01e50f06 type'1)

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_Unsafe_param_undersore_capital@
foreign import ccall unsafe "hs_bindgen_a015d9b3e63c0a07" hs_bindgen_a015d9b3e63c0a07_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_Unsafe_param_undersore_capital@
hs_bindgen_a015d9b3e63c0a07 ::
     RIP.Ptr T
  -> IO ()
hs_bindgen_a015d9b3e63c0a07 =
  RIP.fromFFIType hs_bindgen_a015d9b3e63c0a07_base

{-| __C declaration:__ @param_undersore_capital@

    __defined at:__ @edge-cases\/mangle_fun_param_names.h 11:6@

    __exported by:__ @edge-cases\/mangle_fun_param_names.h@
-}
param_undersore_capital ::
     T
     -- ^ __C declaration:__ @_T@
  -> IO ()
param_undersore_capital =
  \_T0 ->
    RIP.with _T0 (\_T1 ->
                    hs_bindgen_a015d9b3e63c0a07 _T1)

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_Unsafe_param_haskell_reserved_name@
foreign import ccall unsafe "hs_bindgen_6ac28bcdfd5bcd07" hs_bindgen_6ac28bcdfd5bcd07_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_Unsafe_param_haskell_reserved_name@
hs_bindgen_6ac28bcdfd5bcd07 ::
     RIP.Ptr T
  -> IO ()
hs_bindgen_6ac28bcdfd5bcd07 =
  RIP.fromFFIType hs_bindgen_6ac28bcdfd5bcd07_base

{-| __C declaration:__ @param_haskell_reserved_name@

    __defined at:__ @edge-cases\/mangle_fun_param_names.h 12:6@

    __exported by:__ @edge-cases\/mangle_fun_param_names.h@
-}
param_haskell_reserved_name ::
     T
     -- ^ __C declaration:__ @type@
  -> IO ()
param_haskell_reserved_name =
  \type'0 ->
    RIP.with type'0 (\type'1 ->
                       hs_bindgen_6ac28bcdfd5bcd07 type'1)
