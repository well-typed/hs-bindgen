{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <names.h>\nvoid test_internal_by (void) { by(); }\nvoid test_internal_forall (void) { forall(); }\nvoid test_internal_mdo (void) { mdo(); }\nvoid test_internal_pattern (void) { pattern(); }\nvoid test_internal_proc (void) { proc(); }\nvoid test_internal_rec (void) { rec(); }\nvoid test_internal_using (void) { using(); }\nvoid test_internal_anyclass (void) { anyclass(); }\nvoid test_internal_capi (void) { capi(); }\nvoid test_internal_cases (void) { cases(); }\nvoid test_internal_ccall (void) { ccall(); }\nvoid test_internal_dynamic (void) { dynamic(); }\nvoid test_internal_export (void) { export(); }\nvoid test_internal_family (void) { family(); }\nvoid test_internal_group (void) { group(); }\nvoid test_internal_interruptible (void) { interruptible(); }\nvoid test_internal_javascript (void) { javascript(); }\nvoid test_internal_label (void) { label(); }\nvoid test_internal_prim (void) { prim(); }\nvoid test_internal_role (void) { role(); }\nvoid test_internal_safe (void) { safe(); }\nvoid test_internal_stdcall (void) { stdcall(); }\nvoid test_internal_stock (void) { stock(); }\nvoid test_internal_unsafe (void) { unsafe(); }\nvoid test_internal_via (void) { via(); }\n")

foreign import ccall safe "test_internal_by" by' :: IO ()

foreign import ccall safe "test_internal_forall" forall' :: IO ()

foreign import ccall safe "test_internal_mdo" mdo' :: IO ()

foreign import ccall safe "test_internal_pattern" pattern' :: IO ()

foreign import ccall safe "test_internal_proc" proc' :: IO ()

foreign import ccall safe "test_internal_rec" rec' :: IO ()

foreign import ccall safe "test_internal_using" using' :: IO ()

foreign import ccall safe "test_internal_anyclass" anyclass :: IO ()

foreign import ccall safe "test_internal_capi" capi :: IO ()

foreign import ccall safe "test_internal_cases" cases :: IO ()

foreign import ccall safe "test_internal_ccall" ccall :: IO ()

foreign import ccall safe "test_internal_dynamic" dynamic :: IO ()

foreign import ccall safe "test_internal_export" export :: IO ()

foreign import ccall safe "test_internal_family" family :: IO ()

foreign import ccall safe "test_internal_group" group :: IO ()

foreign import ccall safe "test_internal_interruptible" interruptible :: IO ()

foreign import ccall safe "test_internal_javascript" javascript :: IO ()

foreign import ccall safe "test_internal_label" label :: IO ()

foreign import ccall safe "test_internal_prim" prim :: IO ()

foreign import ccall safe "test_internal_role" role :: IO ()

foreign import ccall safe "test_internal_safe" safe :: IO ()

foreign import ccall safe "test_internal_stdcall" stdcall :: IO ()

foreign import ccall safe "test_internal_stock" stock :: IO ()

foreign import ccall safe "test_internal_unsafe" unsafe :: IO ()

foreign import ccall safe "test_internal_via" via :: IO ()
