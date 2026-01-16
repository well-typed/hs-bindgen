{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}

module Example where

import qualified Foreign.C as FC

{-| __C declaration:__ @TOPLEVEL_ANON_A@

    __defined at:__ @types\/enums\/anon_enum_toplevel.h 5:1@

    __exported by:__ @types\/enums\/anon_enum_toplevel.h@
-}
pattern TOPLEVEL_ANON_A :: FC.CUInt
pattern TOPLEVEL_ANON_A = 0

{-| __C declaration:__ @TOPLEVEL_ANON_B@

    __defined at:__ @types\/enums\/anon_enum_toplevel.h 5:1@

    __exported by:__ @types\/enums\/anon_enum_toplevel.h@
-}
pattern TOPLEVEL_ANON_B :: FC.CUInt
pattern TOPLEVEL_ANON_B = 1

{-| __C declaration:__ @TOPLEVEL_ANON_C@

    __defined at:__ @types\/enums\/anon_enum_toplevel.h 5:1@

    __exported by:__ @types\/enums\/anon_enum_toplevel.h@
-}
pattern TOPLEVEL_ANON_C :: FC.CUInt
pattern TOPLEVEL_ANON_C = 100
