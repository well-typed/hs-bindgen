{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/qualifiers/type_qualifiers.h>"
  , "_Bool hs_bindgen_test_typesqualifierstype_qualifie_41af05ef1797fa6d ("
  , "  char const **arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return list_example(arg1, arg2);"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_typesqualifierstype_qualifie_41af05ef1797fa6d" list_example_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr (Ptr.Ptr FC.CChar)
    -> HsBindgen.Runtime.Prelude.CSize
    -> IO FC.CBool
    )

{-| __C declaration:__ @list_example@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:14:6@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
list_example ::
     Ptr.Ptr (Ptr.Ptr FC.CChar)
     {- ^ __C declaration:__ @items@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @count@
     -}
  -> IO FC.CBool
list_example =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType list_example_base
