{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include \"names.h\"\nvoid testmodule_by (void) { by(); }\nvoid testmodule_forall (void) { forall(); }\nvoid testmodule_mdo (void) { mdo(); }\nvoid testmodule_pattern (void) { pattern(); }\nvoid testmodule_proc (void) { proc(); }\nvoid testmodule_rec (void) { rec(); }\nvoid testmodule_using (void) { using(); }\nvoid testmodule_anyclass (void) { anyclass(); }\nvoid testmodule_capi (void) { capi(); }\nvoid testmodule_cases (void) { cases(); }\nvoid testmodule_ccall (void) { ccall(); }\nvoid testmodule_dynamic (void) { dynamic(); }\nvoid testmodule_export (void) { export(); }\nvoid testmodule_family (void) { family(); }\nvoid testmodule_group (void) { group(); }\nvoid testmodule_interruptible (void) { interruptible(); }\nvoid testmodule_javascript (void) { javascript(); }\nvoid testmodule_label (void) { label(); }\nvoid testmodule_prim (void) { prim(); }\nvoid testmodule_role (void) { role(); }\nvoid testmodule_safe (void) { safe(); }\nvoid testmodule_stdcall (void) { stdcall(); }\nvoid testmodule_stock (void) { stock(); }\nvoid testmodule_unsafe (void) { unsafe(); }\nvoid testmodule_via (void) { via(); }\n")

foreign import ccall safe "testmodule_by" by' :: IO ()

foreign import ccall safe "testmodule_forall" forall' :: IO ()

foreign import ccall safe "testmodule_mdo" mdo' :: IO ()

foreign import ccall safe "testmodule_pattern" pattern' :: IO ()

foreign import ccall safe "testmodule_proc" proc' :: IO ()

foreign import ccall safe "testmodule_rec" rec' :: IO ()

foreign import ccall safe "testmodule_using" using' :: IO ()

foreign import ccall safe "testmodule_anyclass" anyclass :: IO ()

foreign import ccall safe "testmodule_capi" capi :: IO ()

foreign import ccall safe "testmodule_cases" cases :: IO ()

foreign import ccall safe "testmodule_ccall" ccall :: IO ()

foreign import ccall safe "testmodule_dynamic" dynamic :: IO ()

foreign import ccall safe "testmodule_export" export :: IO ()

foreign import ccall safe "testmodule_family" family :: IO ()

foreign import ccall safe "testmodule_group" group :: IO ()

foreign import ccall safe "testmodule_interruptible" interruptible :: IO ()

foreign import ccall safe "testmodule_javascript" javascript :: IO ()

foreign import ccall safe "testmodule_label" label :: IO ()

foreign import ccall safe "testmodule_prim" prim :: IO ()

foreign import ccall safe "testmodule_role" role :: IO ()

foreign import ccall safe "testmodule_safe" safe :: IO ()

foreign import ccall safe "testmodule_stdcall" stdcall :: IO ()

foreign import ccall safe "testmodule_stock" stock :: IO ()

foreign import ccall safe "testmodule_unsafe" unsafe :: IO ()

foreign import ccall safe "testmodule_via" via :: IO ()
