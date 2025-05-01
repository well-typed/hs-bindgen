{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import Prelude (IO)

-- #include "names.h"

-- void by (void)

foreign import capi safe "names.h by" by' :: IO ()

-- void forall (void)

foreign import capi safe "names.h forall" forall' :: IO ()

-- void mdo (void)

foreign import capi safe "names.h mdo" mdo' :: IO ()

-- void pattern (void)

foreign import capi safe "names.h pattern" pattern' :: IO ()

-- void proc (void)

foreign import capi safe "names.h proc" proc' :: IO ()

-- void rec (void)

foreign import capi safe "names.h rec" rec' :: IO ()

-- void using (void)

foreign import capi safe "names.h using" using' :: IO ()

-- void anyclass (void)

foreign import capi safe "names.h anyclass" anyclass :: IO ()

-- void capi (void)

foreign import capi safe "names.h capi" capi :: IO ()

-- void cases (void)

foreign import capi safe "names.h cases" cases :: IO ()

-- void ccall (void)

foreign import capi safe "names.h ccall" ccall :: IO ()

-- void dynamic (void)

foreign import capi safe "names.h dynamic" dynamic :: IO ()

-- void export (void)

foreign import capi safe "names.h export" export :: IO ()

-- void family (void)

foreign import capi safe "names.h family" family :: IO ()

-- void group (void)

foreign import capi safe "names.h group" group :: IO ()

-- void interruptible (void)

foreign import capi safe "names.h interruptible" interruptible :: IO ()

-- void javascript (void)

foreign import capi safe "names.h javascript" javascript :: IO ()

-- void label (void)

foreign import capi safe "names.h label" label :: IO ()

-- void prim (void)

foreign import capi safe "names.h prim" prim :: IO ()

-- void role (void)

foreign import capi safe "names.h role" role :: IO ()

-- void safe (void)

foreign import capi safe "names.h safe" safe :: IO ()

-- void stdcall (void)

foreign import capi safe "names.h stdcall" stdcall :: IO ()

-- void stock (void)

foreign import capi safe "names.h stock" stock :: IO ()

-- void unsafe (void)

foreign import capi safe "names.h unsafe" unsafe :: IO ()

-- void via (void)

foreign import capi safe "names.h via" via :: IO ()
