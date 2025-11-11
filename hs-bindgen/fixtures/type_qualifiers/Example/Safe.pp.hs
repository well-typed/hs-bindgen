{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <type_qualifiers.h>"
  , "_Bool hs_bindgen_test_type_qualifiers_b42fb41209c21d6e ("
  , "  char const **arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return list_example(arg1, arg2);"
  , "}"
  ]))

{-| __C declaration:__ @list_example@

    __defined at:__ @type_qualifiers.h:14:6@

    __exported by:__ @type_qualifiers.h@
-}
foreign import ccall safe "hs_bindgen_test_type_qualifiers_b42fb41209c21d6e" list_example ::
     Ptr.Ptr (Ptr.Ptr FC.CChar)
     {- ^ __C declaration:__ @items@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @count@
     -}
  -> IO FC.CBool
