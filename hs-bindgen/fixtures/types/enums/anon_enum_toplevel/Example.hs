{-# LANGUAGE PatternSynonyms #-}

module Example where

import qualified HsBindgen.Runtime.Internal.Prelude as RIP

{-| __C declaration:__ @TOPLEVEL_ANON_A@

    __defined at:__ @types\/enums\/anon_enum_toplevel.h 5:1@

    __exported by:__ @types\/enums\/anon_enum_toplevel.h@
-}
pattern TOPLEVEL_ANON_A :: RIP.CUInt
pattern TOPLEVEL_ANON_A = 0

{-| __C declaration:__ @TOPLEVEL_ANON_B@

    __defined at:__ @types\/enums\/anon_enum_toplevel.h 5:1@

    __exported by:__ @types\/enums\/anon_enum_toplevel.h@
-}
pattern TOPLEVEL_ANON_B :: RIP.CUInt
pattern TOPLEVEL_ANON_B = 1

{-| __C declaration:__ @TOPLEVEL_ANON_C@

    __defined at:__ @types\/enums\/anon_enum_toplevel.h 5:1@

    __exported by:__ @types\/enums\/anon_enum_toplevel.h@
-}
pattern TOPLEVEL_ANON_C :: RIP.CUInt
pattern TOPLEVEL_ANON_C = 100
