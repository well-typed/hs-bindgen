{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.register_handler
    , Example.Safe.visit
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <types/boot/internal_module.h>"
  , "void hs_bindgen_31a9ed072676f8c3 ("
  , "  on_event arg1"
  , ")"
  , "{"
  , "  (register_handler)(arg1);"
  , "}"
  , "void hs_bindgen_729369c12053db70 ("
  , "  signed int (*arg1) ("
  , "  struct point *arg1"
  , ")"
  , ")"
  , "{"
  , "  (visit)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_typesbootinternal_module_Example_Safe_register_handler@
foreign import ccall safe "hs_bindgen_31a9ed072676f8c3" hs_bindgen_31a9ed072676f8c3_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_typesbootinternal_module_Example_Safe_register_handler@
hs_bindgen_31a9ed072676f8c3 ::
     On_event
  -> IO ()
hs_bindgen_31a9ed072676f8c3 =
  \x0 ->
    hs_bindgen_31a9ed072676f8c3_base (BG.toFFIType x0)

{-| __C declaration:__ @register_handler@

    __defined at:__ @types\/boot\/internal_module.h 23:6@

    __exported by:__ @types\/boot\/internal_module.h@
-}
register_handler ::
     On_event
     -- ^ __C declaration:__ @f@
  -> IO ()
register_handler = hs_bindgen_31a9ed072676f8c3

-- __unique:__ @test_typesbootinternal_module_Example_Safe_visit@
foreign import ccall safe "hs_bindgen_729369c12053db70" hs_bindgen_729369c12053db70_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_typesbootinternal_module_Example_Safe_visit@
hs_bindgen_729369c12053db70 ::
     BG.FunPtr (BG.Ptr Point -> IO BG.CInt)
  -> IO ()
hs_bindgen_729369c12053db70 =
  \x0 ->
    hs_bindgen_729369c12053db70_base (BG.toFFIType x0)

{-| __C declaration:__ @visit@

    __defined at:__ @types\/boot\/internal_module.h 34:6@

    __exported by:__ @types\/boot\/internal_module.h@
-}
visit ::
     BG.FunPtr (BG.Ptr Point -> IO BG.CInt)
     -- ^ __C declaration:__ @f@
  -> IO ()
visit = hs_bindgen_729369c12053db70
