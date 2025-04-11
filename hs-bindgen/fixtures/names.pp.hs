{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import Prelude (IO)

foreign import capi safe "names.h capi" capi :: IO ()

foreign import capi safe "names.h ccall" ccall :: IO ()

foreign import capi safe "names.h interruptible" interruptible :: IO ()

foreign import capi safe "names.h javascript" javascript :: IO ()

foreign import capi safe "names.h prim" prim :: IO ()

foreign import capi safe "names.h safe" safe :: IO ()

foreign import capi safe "names.h stdcall" stdcall :: IO ()

foreign import capi safe "names.h unsafe" unsafe :: IO ()

foreign import capi safe "names.h using" using :: IO ()

foreign import capi safe "names.h via" via :: IO ()
