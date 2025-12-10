{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <declarations/declarations_required_for_scoping.h>"
  , "/* test_declarationsdeclarations_requ_Example_get_f_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2d056172b6e93949 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &f;"
  , "}"
  ]))

-- | __unique:__ @test_declarationsdeclarations_requ_Example_get_f_ptr@
foreign import ccall unsafe "hs_bindgen_2d056172b6e93949" hs_bindgen_2d056172b6e93949 ::
     IO (Ptr.FunPtr (A -> IO ()))

{-# NOINLINE f_ptr #-}

{-| __C declaration:__ @f@

    __defined at:__ @declarations\/declarations_required_for_scoping.h:7:6@

    __exported by:__ @declarations\/declarations_required_for_scoping.h@
-}
f_ptr :: Ptr.FunPtr (A -> IO ())
f_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2d056172b6e93949
