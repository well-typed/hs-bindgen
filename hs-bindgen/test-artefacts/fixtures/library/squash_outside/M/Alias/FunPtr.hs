{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Alias.FunPtr
    ( M.Alias.FunPtr.alias_show
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import M.Alias
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <root.h>"
  , "/* test_squash_outside_M.Alias_get_alias_show */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cbd3102be8ab3c32 (void)) ("
  , "  window *arg1"
  , ")"
  , "{"
  , "  return &alias_show;"
  , "}"
  ]))

-- __unique:__ @test_squash_outside_M.Alias_get_alias_show@
foreign import ccall unsafe "hs_bindgen_cbd3102be8ab3c32" hs_bindgen_cbd3102be8ab3c32_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_squash_outside_M.Alias_get_alias_show@
hs_bindgen_cbd3102be8ab3c32 :: IO (BG.FunPtr (BG.Ptr Window -> IO ()))
hs_bindgen_cbd3102be8ab3c32 =
  fmap BG.fromFFIType hs_bindgen_cbd3102be8ab3c32_base

{-# NOINLINE alias_show #-}
{-| __C declaration:__ @alias_show@

    __defined at:__ @lib\/alias.h 6:6@

    __exported by:__ @root.h@
-}
alias_show :: BG.FunPtr (BG.Ptr Window -> IO ())
alias_show =
  BG.unsafePerformIO hs_bindgen_cbd3102be8ab3c32
