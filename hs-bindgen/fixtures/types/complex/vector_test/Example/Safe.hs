{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/complex/vector_test.h>"
  , "vector *hs_bindgen_cd5f566bc96dcba0 ("
  , "  double arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return new_vector(arg1, arg2);"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_cd5f566bc96dcba0" new_vector_base ::
     FC.CDouble
  -> FC.CDouble
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @new_vector@

    __defined at:__ @types\/complex\/vector_test.h:6:9@

    __exported by:__ @types\/complex\/vector_test.h@

    __unique:__ @test_typescomplexvector_test_Example_Safe_new_vector@
-}
new_vector ::
     FC.CDouble
     -- ^ __C declaration:__ @x@
  -> FC.CDouble
     -- ^ __C declaration:__ @y@
  -> IO (Ptr.Ptr Vector)
new_vector =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType new_vector_base
