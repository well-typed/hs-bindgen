{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ModuleNames.C2d.Leading_digit
    ( ModuleNames.C2d.Leading_digit.lEADING_DIGIT_MAX
    )
  where

import qualified C.Expr.HostPlatform
import qualified HsBindgen.Runtime.Support as BG
import qualified ModuleNames.Same_basename

{-| __C declaration:__ @macro LEADING_DIGIT_MAX@

    __defined at:__ @module_names\/2d\/leading_digit.h 11:9@

    __exported by:__ @module_names.h@
-}
lEADING_DIGIT_MAX :: BG.CInt
lEADING_DIGIT_MAX =
  (C.Expr.HostPlatform./) ModuleNames.Same_basename.sAME_BASENAME_MAX (2 :: BG.CInt)
