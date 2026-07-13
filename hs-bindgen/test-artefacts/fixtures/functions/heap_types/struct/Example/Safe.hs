{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.fun
    , Example.Safe.param_underscore
    , Example.Safe.param_uppercase
    , Example.Safe.param_undersore_capital
    , Example.Safe.param_haskell_reserved_name
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/heap_types/struct.h>"
  , "void hs_bindgen_a5d53f538e59b1fc ("
  , "  T *arg1,"
  , "  T *arg2"
  , ")"
  , "{"
  , "  *arg2 = (fun)(*arg1);"
  , "}"
  , "void hs_bindgen_8b003c42270f977d ("
  , "  T *arg1"
  , ")"
  , "{"
  , "  (param_underscore)(*arg1);"
  , "}"
  , "void hs_bindgen_820d4de13c1d8dff ("
  , "  T *arg1"
  , ")"
  , "{"
  , "  (param_uppercase)(*arg1);"
  , "}"
  , "void hs_bindgen_aa622269ffe58e15 ("
  , "  T *arg1"
  , ")"
  , "{"
  , "  (param_undersore_capital)(*arg1);"
  , "}"
  , "void hs_bindgen_40fff9d72335931a ("
  , "  T *arg1"
  , ")"
  , "{"
  , "  (param_haskell_reserved_name)(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsheap_typesstruct_Example_Safe_fun@
foreign import ccall safe "hs_bindgen_a5d53f538e59b1fc" hs_bindgen_a5d53f538e59b1fc_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_functionsheap_typesstruct_Example_Safe_fun@
hs_bindgen_a5d53f538e59b1fc ::
     BG.Ptr T
  -> BG.Ptr T
  -> IO ()
hs_bindgen_a5d53f538e59b1fc =
  BG.fromFFIType hs_bindgen_a5d53f538e59b1fc_base

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
    BG.with x0 (\x1 ->
                  BG.allocaAndPeek (\res2 ->
                                      hs_bindgen_a5d53f538e59b1fc x1 res2))

-- __unique:__ @test_functionsheap_typesstruct_Example_Safe_param_underscore@
foreign import ccall safe "hs_bindgen_8b003c42270f977d" hs_bindgen_8b003c42270f977d_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_functionsheap_typesstruct_Example_Safe_param_underscore@
hs_bindgen_8b003c42270f977d ::
     BG.Ptr T
  -> IO ()
hs_bindgen_8b003c42270f977d =
  BG.fromFFIType hs_bindgen_8b003c42270f977d_base

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
    BG.with _0 (\_1 -> hs_bindgen_8b003c42270f977d _1)

-- __unique:__ @test_functionsheap_typesstruct_Example_Safe_param_uppercase@
foreign import ccall safe "hs_bindgen_820d4de13c1d8dff" hs_bindgen_820d4de13c1d8dff_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_functionsheap_typesstruct_Example_Safe_param_uppercase@
hs_bindgen_820d4de13c1d8dff ::
     BG.Ptr T
  -> IO ()
hs_bindgen_820d4de13c1d8dff =
  BG.fromFFIType hs_bindgen_820d4de13c1d8dff_base

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
    BG.with type'0 (\type'1 ->
                      hs_bindgen_820d4de13c1d8dff type'1)

-- __unique:__ @test_functionsheap_typesstruct_Example_Safe_param_undersore_capital@
foreign import ccall safe "hs_bindgen_aa622269ffe58e15" hs_bindgen_aa622269ffe58e15_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_functionsheap_typesstruct_Example_Safe_param_undersore_capital@
hs_bindgen_aa622269ffe58e15 ::
     BG.Ptr T
  -> IO ()
hs_bindgen_aa622269ffe58e15 =
  BG.fromFFIType hs_bindgen_aa622269ffe58e15_base

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
    BG.with _T0 (\_T1 -> hs_bindgen_aa622269ffe58e15 _T1)

-- __unique:__ @test_functionsheap_typesstruct_Example_Safe_param_haskell_reserved_name@
foreign import ccall safe "hs_bindgen_40fff9d72335931a" hs_bindgen_40fff9d72335931a_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_functionsheap_typesstruct_Example_Safe_param_haskell_reserved_name@
hs_bindgen_40fff9d72335931a ::
     BG.Ptr T
  -> IO ()
hs_bindgen_40fff9d72335931a =
  BG.fromFFIType hs_bindgen_40fff9d72335931a_base

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
    BG.with type'0 (\type'1 ->
                      hs_bindgen_40fff9d72335931a type'1)
