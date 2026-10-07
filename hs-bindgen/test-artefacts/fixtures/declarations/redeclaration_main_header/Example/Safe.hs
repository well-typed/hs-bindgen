{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.f
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <declarations/redeclaration_main_header.h>"
  , "signed int hs_bindgen_8fd6b7730e61d30c ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (f)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_declarationsredeclaration_mai_Example_Safe_f@
foreign import ccall safe "hs_bindgen_8fd6b7730e61d30c" hs_bindgen_8fd6b7730e61d30c_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_declarationsredeclaration_mai_Example_Safe_f@
hs_bindgen_8fd6b7730e61d30c ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_8fd6b7730e61d30c =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_8fd6b7730e61d30c_base (BG.toFFIType x0))

{-| __C declaration:__ @f@

    __defined at:__ @declarations\/redeclaration_main_header.h 5:5@

    __exported by:__ @declarations\/redeclaration_main_header.h@
-}
f ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f = hs_bindgen_8fd6b7730e61d30c
