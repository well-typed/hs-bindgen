{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <vector_test.h>"
  , "vector *hs_bindgen_test_vector_test_c8cd49ec7dbcac25 ("
  , "  double arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return new_vector(arg1, arg2);"
  , "}"
  ]))

{-| __C declaration:__ @new_vector@

    __defined at:__ @vector_test.h:6:9@

    __exported by:__ @vector_test.h@
-}
foreign import ccall safe "hs_bindgen_test_vector_test_c8cd49ec7dbcac25" new_vector ::
     FC.CDouble
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @y@
     -}
  -> IO (Ptr.Ptr Vector)
