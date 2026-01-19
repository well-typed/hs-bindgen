{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/flam.h>"
  , "signed int hs_bindgen_271cf433c1a3762f ("
  , "  struct vector const *arg1,"
  , "  struct vector *arg2"
  , ")"
  , "{"
  , "  return reverse(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesflam_Example_Unsafe_reverse@
foreign import ccall unsafe "hs_bindgen_271cf433c1a3762f" hs_bindgen_271cf433c1a3762f ::
     HsBindgen.Runtime.ConstPtr.ConstPtr Vector
  -> Ptr.Ptr Vector
  -> IO FC.CInt

{-| __C declaration:__ @reverse@

    __defined at:__ @edge-cases\/flam.h 36:12@

    __exported by:__ @edge-cases\/flam.h@
-}
reverse ::
     HsBindgen.Runtime.ConstPtr.ConstPtr Vector
     -- ^ __C declaration:__ @input@
  -> Ptr.Ptr Vector
     -- ^ __C declaration:__ @output@
  -> IO FC.CInt
reverse = hs_bindgen_271cf433c1a3762f
