{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/qualifiers/type_qualifiers.h>"
  , "/* test_typesqualifierstype_qualifie_Example_get_a */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_b9f6c3995e03a64f (void)"
  , "{"
  , "  return &a;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_b */"
  , "__attribute__ ((const))"
  , "signed int const **hs_bindgen_2a09e45ccb76e4da (void)"
  , "{"
  , "  return &b;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_c */"
  , "__attribute__ ((const))"
  , "signed int *const *hs_bindgen_9cef5885d51a5077 (void)"
  , "{"
  , "  return &c;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_d */"
  , "__attribute__ ((const))"
  , "signed int const *const *hs_bindgen_039fc3584df4cf95 (void)"
  , "{"
  , "  return &d;"
  , "}"
  ]))

{-| __unique:__ @test_typesqualifierstype_qualifie_Example_get_a@
-}
foreign import ccall unsafe "hs_bindgen_b9f6c3995e03a64f" hs_bindgen_b9f6c3995e03a64f ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)

{-# NOINLINE hs_bindgen_568187d7e5f899e0 #-}

{-| __C declaration:__ @a@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:5:18@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
hs_bindgen_568187d7e5f899e0 :: HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
hs_bindgen_568187d7e5f899e0 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b9f6c3995e03a64f

{-# NOINLINE a #-}

a :: FC.CInt
a =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_568187d7e5f899e0))

{-| __unique:__ @test_typesqualifierstype_qualifie_Example_get_b@
-}
foreign import ccall unsafe "hs_bindgen_2a09e45ccb76e4da" hs_bindgen_2a09e45ccb76e4da ::
     IO (Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))

{-# NOINLINE b #-}

{-| __C declaration:__ @b@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:7:19@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
b :: Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
b =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2a09e45ccb76e4da

{-| __unique:__ @test_typesqualifierstype_qualifie_Example_get_c@
-}
foreign import ccall unsafe "hs_bindgen_9cef5885d51a5077" hs_bindgen_9cef5885d51a5077 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr (Ptr.Ptr FC.CInt))

{-# NOINLINE hs_bindgen_86ab1f1ce8db256e #-}

{-| __C declaration:__ @c@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:9:19@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
hs_bindgen_86ab1f1ce8db256e :: HsBindgen.Runtime.ConstPtr.ConstPtr (Ptr.Ptr FC.CInt)
hs_bindgen_86ab1f1ce8db256e =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9cef5885d51a5077

{-# NOINLINE c #-}

c :: Ptr.Ptr FC.CInt
c =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_86ab1f1ce8db256e))

{-| __unique:__ @test_typesqualifierstype_qualifie_Example_get_d@
-}
foreign import ccall unsafe "hs_bindgen_039fc3584df4cf95" hs_bindgen_039fc3584df4cf95 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))

{-# NOINLINE hs_bindgen_f5892dac29d00ba8 #-}

{-| __C declaration:__ @d@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:11:25@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
hs_bindgen_f5892dac29d00ba8 :: HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
hs_bindgen_f5892dac29d00ba8 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_039fc3584df4cf95

{-# NOINLINE d #-}

d :: HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
d =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_f5892dac29d00ba8))
