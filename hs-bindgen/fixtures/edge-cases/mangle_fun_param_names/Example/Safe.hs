{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.param_underscore
    , Example.Safe.param_uppercase
    , Example.Safe.param_undersore_capital
    , Example.Safe.param_haskell_reserved_name
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <edge-cases/mangle_fun_param_names.h>"
  , "void hs_bindgen_32fb10fccaa5c64b ("
  , "  T *arg1"
  , ")"
  , "{"
  , "  (param_underscore)(*arg1);"
  , "}"
  , "void hs_bindgen_5888c48ab8bc55c6 ("
  , "  T *arg1"
  , ")"
  , "{"
  , "  (param_uppercase)(*arg1);"
  , "}"
  , "void hs_bindgen_330a5861a2d663d1 ("
  , "  T *arg1"
  , ")"
  , "{"
  , "  (param_undersore_capital)(*arg1);"
  , "}"
  , "void hs_bindgen_8db88cfd2f21fdb1 ("
  , "  T *arg1"
  , ")"
  , "{"
  , "  (param_haskell_reserved_name)(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_Safe_param_underscore@
foreign import ccall safe "hs_bindgen_32fb10fccaa5c64b" hs_bindgen_32fb10fccaa5c64b_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_Safe_param_underscore@
hs_bindgen_32fb10fccaa5c64b ::
     RIP.Ptr T
  -> IO ()
hs_bindgen_32fb10fccaa5c64b =
  RIP.fromFFIType hs_bindgen_32fb10fccaa5c64b_base

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
    RIP.with _0 (\_1 -> hs_bindgen_32fb10fccaa5c64b _1)

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_Safe_param_uppercase@
foreign import ccall safe "hs_bindgen_5888c48ab8bc55c6" hs_bindgen_5888c48ab8bc55c6_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_Safe_param_uppercase@
hs_bindgen_5888c48ab8bc55c6 ::
     RIP.Ptr T
  -> IO ()
hs_bindgen_5888c48ab8bc55c6 =
  RIP.fromFFIType hs_bindgen_5888c48ab8bc55c6_base

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
                       hs_bindgen_5888c48ab8bc55c6 type'1)

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_Safe_param_undersore_capital@
foreign import ccall safe "hs_bindgen_330a5861a2d663d1" hs_bindgen_330a5861a2d663d1_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_Safe_param_undersore_capital@
hs_bindgen_330a5861a2d663d1 ::
     RIP.Ptr T
  -> IO ()
hs_bindgen_330a5861a2d663d1 =
  RIP.fromFFIType hs_bindgen_330a5861a2d663d1_base

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
                    hs_bindgen_330a5861a2d663d1 _T1)

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_Safe_param_haskell_reserved_name@
foreign import ccall safe "hs_bindgen_8db88cfd2f21fdb1" hs_bindgen_8db88cfd2f21fdb1_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_Safe_param_haskell_reserved_name@
hs_bindgen_8db88cfd2f21fdb1 ::
     RIP.Ptr T
  -> IO ()
hs_bindgen_8db88cfd2f21fdb1 =
  RIP.fromFFIType hs_bindgen_8db88cfd2f21fdb1_base

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
                       hs_bindgen_8db88cfd2f21fdb1 type'1)
