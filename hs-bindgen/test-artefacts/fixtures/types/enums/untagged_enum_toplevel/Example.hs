{-# LANGUAGE PatternSynonyms #-}

module Example
    ( pattern Example.TOPLEVEL_UNTAGGED_A
    , pattern Example.TOPLEVEL_UNTAGGED_B
    , pattern Example.TOPLEVEL_UNTAGGED_C
    )
  where

import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @TOPLEVEL_UNTAGGED_A@

    __defined at:__ @types\/enums\/untagged_enum_toplevel.h 5:1@

    __exported by:__ @types\/enums\/untagged_enum_toplevel.h@
-}
pattern TOPLEVEL_UNTAGGED_A :: BG.CUInt
pattern TOPLEVEL_UNTAGGED_A = 0

{-| __C declaration:__ @TOPLEVEL_UNTAGGED_B@

    __defined at:__ @types\/enums\/untagged_enum_toplevel.h 5:1@

    __exported by:__ @types\/enums\/untagged_enum_toplevel.h@
-}
pattern TOPLEVEL_UNTAGGED_B :: BG.CUInt
pattern TOPLEVEL_UNTAGGED_B = 1

{-| __C declaration:__ @TOPLEVEL_UNTAGGED_C@

    __defined at:__ @types\/enums\/untagged_enum_toplevel.h 5:1@

    __exported by:__ @types\/enums\/untagged_enum_toplevel.h@
-}
pattern TOPLEVEL_UNTAGGED_C :: BG.CUInt
pattern TOPLEVEL_UNTAGGED_C = 100
