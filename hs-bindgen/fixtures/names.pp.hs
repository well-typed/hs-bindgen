{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import Prelude (IO)

-- #include "names.h"
-- void testmodule_by (void);
-- void testmodule_forall (void);
-- void testmodule_mdo (void);
-- void testmodule_pattern (void);
-- void testmodule_proc (void);
-- void testmodule_rec (void);
-- void testmodule_using (void);
-- void testmodule_anyclass (void);
-- void testmodule_capi (void);
-- void testmodule_cases (void);
-- void testmodule_ccall (void);
-- void testmodule_dynamic (void);
-- void testmodule_export (void);
-- void testmodule_family (void);
-- void testmodule_group (void);
-- void testmodule_interruptible (void);
-- void testmodule_javascript (void);
-- void testmodule_label (void);
-- void testmodule_prim (void);
-- void testmodule_role (void);
-- void testmodule_safe (void);
-- void testmodule_stdcall (void);
-- void testmodule_stock (void);
-- void testmodule_unsafe (void);
-- void testmodule_via (void);

foreign import capi safe "names.h by" by' :: IO ()

foreign import capi safe "names.h forall" forall' :: IO ()

foreign import capi safe "names.h mdo" mdo' :: IO ()

foreign import capi safe "names.h pattern" pattern' :: IO ()

foreign import capi safe "names.h proc" proc' :: IO ()

foreign import capi safe "names.h rec" rec' :: IO ()

foreign import capi safe "names.h using" using' :: IO ()

foreign import capi safe "names.h anyclass" anyclass :: IO ()

foreign import capi safe "names.h capi" capi :: IO ()

foreign import capi safe "names.h cases" cases :: IO ()

foreign import capi safe "names.h ccall" ccall :: IO ()

foreign import capi safe "names.h dynamic" dynamic :: IO ()

foreign import capi safe "names.h export" export :: IO ()

foreign import capi safe "names.h family" family :: IO ()

foreign import capi safe "names.h group" group :: IO ()

foreign import capi safe "names.h interruptible" interruptible :: IO ()

foreign import capi safe "names.h javascript" javascript :: IO ()

foreign import capi safe "names.h label" label :: IO ()

foreign import capi safe "names.h prim" prim :: IO ()

foreign import capi safe "names.h role" role :: IO ()

foreign import capi safe "names.h safe" safe :: IO ()

foreign import capi safe "names.h stdcall" stdcall :: IO ()

foreign import capi safe "names.h stock" stock :: IO ()

foreign import capi safe "names.h unsafe" unsafe :: IO ()

foreign import capi safe "names.h via" via :: IO ()
