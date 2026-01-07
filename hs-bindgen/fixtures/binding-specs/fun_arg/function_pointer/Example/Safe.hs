{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified M1
import qualified M2
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/function_pointer.h>"
  , "void hs_bindgen_2470a43481a1ae0c ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_033bdfbf0dd68eb8 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  bar(arg1);"
  , "}"
  , "void hs_bindgen_0dacce0627b3582f ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  baz(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_2470a43481a1ae0c" hs_bindgen_2470a43481a1ae0c ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
  -> IO ()

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h:3:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
foo ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_2470a43481a1ae0c

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_033bdfbf0dd68eb8" hs_bindgen_033bdfbf0dd68eb8 ::
     Ptr.Ptr M1.A
  -> IO ()

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h:4:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
bar ::
     Ptr.Ptr M1.A
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_033bdfbf0dd68eb8

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_baz@
foreign import ccall safe "hs_bindgen_0dacce0627b3582f" hs_bindgen_0dacce0627b3582f ::
     Ptr.Ptr M2.B
  -> IO ()

{-| __C declaration:__ @baz@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h:5:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
baz ::
     Ptr.Ptr M2.B
     -- ^ __C declaration:__ @x@
  -> IO ()
baz = hs_bindgen_0dacce0627b3582f
