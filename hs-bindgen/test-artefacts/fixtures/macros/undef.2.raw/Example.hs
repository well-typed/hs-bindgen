{-# LANGUAGE NoImplicitPrelude #-}

module Example
    ( Example.t
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro
import Prelude (String)

{-| __C declaration:__ @macro T@

    __defined at:__ @macros\/undef.h 3:9@

    __exported by:__ @macros\/undef.h@
-}
t :: Macro.Raw String
t = Macro.objectLike "T" ["int"]
