{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/qualifiers/type_qualifiers.h>"
  , "_Bool hs_bindgen_32187cc02676ee72 ("
  , "  char const **arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return list_example(arg1, arg2);"
  , "}"
  ]))

{-| __C declaration:__ @list_example@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:14:6@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@

    __unique:__ @test_typesqualifierstype_qualifie_Example_Safe_list_example@
-}
foreign import ccall safe "hs_bindgen_32187cc02676ee72" list_example ::
     Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar)
     -- ^ __C declaration:__ @items@
  -> HsBindgen.Runtime.Prelude.CSize
     -- ^ __C declaration:__ @count@
  -> IO FC.CBool
