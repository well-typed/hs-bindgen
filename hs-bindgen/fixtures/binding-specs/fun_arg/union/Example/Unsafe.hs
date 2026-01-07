{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified M1
import qualified M2
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/union.h>"
  , "void hs_bindgen_82b12ac06ad69760 ("
  , "  union U *arg1"
  , ")"
  , "{"
  , "  foo(*arg1);"
  , "}"
  , "void hs_bindgen_a1f0c2d5b21c4309 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  bar(arg1);"
  , "}"
  , "void hs_bindgen_ef9455549f6b066c ("
  , "  B arg1"
  , ")"
  , "{"
  , "  baz(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argunion_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_82b12ac06ad69760" hs_bindgen_82b12ac06ad69760 ::
     Ptr.Ptr U
  -> IO ()

{-| Pointer-based API for 'foo'
-}
foo_wrapper ::
     Ptr.Ptr U
     -- ^ __C declaration:__ @x@
  -> IO ()
foo_wrapper = hs_bindgen_82b12ac06ad69760

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/union.h:4:6@

    __exported by:__ @binding-specs\/fun_arg\/union.h@
-}
foo ::
     U
     -- ^ __C declaration:__ @x@
  -> IO ()
foo =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_82b12ac06ad69760 y1)

-- __unique:__ @test_bindingspecsfun_argunion_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_a1f0c2d5b21c4309" hs_bindgen_a1f0c2d5b21c4309 ::
     M1.A
  -> IO ()

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/union.h:5:6@

    __exported by:__ @binding-specs\/fun_arg\/union.h@
-}
bar ::
     M1.A
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_a1f0c2d5b21c4309

-- __unique:__ @test_bindingspecsfun_argunion_Example_Unsafe_baz@
foreign import ccall unsafe "hs_bindgen_ef9455549f6b066c" hs_bindgen_ef9455549f6b066c ::
     M2.B
  -> IO ()

{-| __C declaration:__ @baz@

    __defined at:__ @binding-specs\/fun_arg\/union.h:6:6@

    __exported by:__ @binding-specs\/fun_arg\/union.h@
-}
baz ::
     M2.B
     -- ^ __C declaration:__ @x@
  -> IO ()
baz = hs_bindgen_ef9455549f6b066c
