{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.f
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <declarations/redeclaration_main_header.h>"
  , "signed int hs_bindgen_78fcd91e79ff5b5f ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (f)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_declarationsredeclaration_mai_Example_Unsafe_f@
foreign import ccall unsafe "hs_bindgen_78fcd91e79ff5b5f" hs_bindgen_78fcd91e79ff5b5f_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_declarationsredeclaration_mai_Example_Unsafe_f@
hs_bindgen_78fcd91e79ff5b5f ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_78fcd91e79ff5b5f =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_78fcd91e79ff5b5f_base (BG.toFFIType x0))

{-| __C declaration:__ @f@

    __defined at:__ @declarations\/redeclaration_main_header.h 5:5@

    __exported by:__ @declarations\/redeclaration_main_header.h@
-}
f ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f = hs_bindgen_78fcd91e79ff5b5f
