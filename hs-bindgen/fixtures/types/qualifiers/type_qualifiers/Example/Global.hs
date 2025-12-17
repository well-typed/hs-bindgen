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
  , "/* test_typesqualifierstype_qualifie_Example_get_a_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_64d60cae0690e115 (void)"
  , "{"
  , "  return &a;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_b_ptr */"
  , "__attribute__ ((const))"
  , "signed int const **hs_bindgen_e2844224d896e170 (void)"
  , "{"
  , "  return &b;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_c_ptr */"
  , "__attribute__ ((const))"
  , "signed int *const *hs_bindgen_ab653695917fba40 (void)"
  , "{"
  , "  return &c;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_d_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const *hs_bindgen_8e02c92809ce6a69 (void)"
  , "{"
  , "  return &d;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_e_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const **hs_bindgen_7669b7dbb3cbc001 (void)"
  , "{"
  , "  return &e;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_f_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const ***hs_bindgen_8641dc294b52f293 (void)"
  , "{"
  , "  return &f;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_g_ptr */"
  , "__attribute__ ((const))"
  , "signed int const **const **hs_bindgen_07b0abd093294331 (void)"
  , "{"
  , "  return &g;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_h_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const *const *const *hs_bindgen_88a59e84c3c59d06 (void)"
  , "{"
  , "  return &h;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_i_ptr */"
  , "__attribute__ ((const))"
  , "signed int *const **const **hs_bindgen_08866fb8af16d42f (void)"
  , "{"
  , "  return &i;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_j_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const *const ***hs_bindgen_ee2ae365c166ef95 (void)"
  , "{"
  , "  return &j;"
  , "}"
  ]))

-- | __unique:__ @test_typesqualifierstype_qualifie_Example_get_a_ptr@
foreign import ccall unsafe "hs_bindgen_64d60cae0690e115" hs_bindgen_64d60cae0690e115 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)

{-# NOINLINE a_ptr #-}

{-| __C declaration:__ @a@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:5:18@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
a_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
a_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_64d60cae0690e115

{-# NOINLINE a #-}

a :: FC.CInt
a =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr a_ptr))

-- | __unique:__ @test_typesqualifierstype_qualifie_Example_get_b_ptr@
foreign import ccall unsafe "hs_bindgen_e2844224d896e170" hs_bindgen_e2844224d896e170 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))

{-# NOINLINE b_ptr #-}

{-| __C declaration:__ @b@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:7:19@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
b_ptr :: Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
b_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e2844224d896e170

-- | __unique:__ @test_typesqualifierstype_qualifie_Example_get_c_ptr@
foreign import ccall unsafe "hs_bindgen_ab653695917fba40" hs_bindgen_ab653695917fba40 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr (Ptr.Ptr FC.CInt))

{-# NOINLINE c_ptr #-}

{-| __C declaration:__ @c@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:9:19@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
c_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr (Ptr.Ptr FC.CInt)
c_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ab653695917fba40

{-# NOINLINE c #-}

c :: Ptr.Ptr FC.CInt
c =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr c_ptr))

-- | __unique:__ @test_typesqualifierstype_qualifie_Example_get_d_ptr@
foreign import ccall unsafe "hs_bindgen_8e02c92809ce6a69" hs_bindgen_8e02c92809ce6a69 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))

{-# NOINLINE d_ptr #-}

{-| __C declaration:__ @d@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:11:25@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
d_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
d_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8e02c92809ce6a69

{-# NOINLINE d #-}

d :: HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
d =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr d_ptr))

-- | __unique:__ @test_typesqualifierstype_qualifie_Example_get_e_ptr@
foreign import ccall unsafe "hs_bindgen_7669b7dbb3cbc001" hs_bindgen_7669b7dbb3cbc001 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))

{-# NOINLINE e_ptr #-}

{-| __C declaration:__ @e@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:18:26@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
e_ptr :: Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))
e_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7669b7dbb3cbc001

-- | __unique:__ @test_typesqualifierstype_qualifie_Example_get_f_ptr@
foreign import ccall unsafe "hs_bindgen_8641dc294b52f293" hs_bindgen_8641dc294b52f293 ::
     IO (Ptr.Ptr (Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))))

{-# NOINLINE f_ptr #-}

{-| __C declaration:__ @f@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:20:27@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
f_ptr :: Ptr.Ptr (Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))
f_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8641dc294b52f293

-- | __unique:__ @test_typesqualifierstype_qualifie_Example_get_g_ptr@
foreign import ccall unsafe "hs_bindgen_07b0abd093294331" hs_bindgen_07b0abd093294331 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr (Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))))

{-# NOINLINE g_ptr #-}

{-| __C declaration:__ @g@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:22:27@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
g_ptr :: Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr (Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))
g_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_07b0abd093294331

-- | __unique:__ @test_typesqualifierstype_qualifie_Example_get_h_ptr@
foreign import ccall unsafe "hs_bindgen_88a59e84c3c59d06" hs_bindgen_88a59e84c3c59d06 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))))

{-# NOINLINE h_ptr #-}

{-| __C declaration:__ @h@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:24:39@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
h_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))
h_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_88a59e84c3c59d06

{-# NOINLINE h #-}

h :: HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))
h =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr h_ptr))

-- | __unique:__ @test_typesqualifierstype_qualifie_Example_get_i_ptr@
foreign import ccall unsafe "hs_bindgen_08866fb8af16d42f" hs_bindgen_08866fb8af16d42f ::
     IO (Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr (Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr (Ptr.Ptr FC.CInt)))))

{-# NOINLINE i_ptr #-}

{-| __C declaration:__ @i@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:26:28@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
i_ptr :: Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr (Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr (Ptr.Ptr FC.CInt))))
i_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_08866fb8af16d42f

-- | __unique:__ @test_typesqualifierstype_qualifie_Example_get_j_ptr@
foreign import ccall unsafe "hs_bindgen_ee2ae365c166ef95" hs_bindgen_ee2ae365c166ef95 ::
     IO (Ptr.Ptr (Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))))

{-# NOINLINE j_ptr #-}

{-| __C declaration:__ @j@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:28:34@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
j_ptr :: Ptr.Ptr (Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))))
j_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ee2ae365c166ef95
