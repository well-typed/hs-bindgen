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
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
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
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_64d60cae0690e115" hs_bindgen_64d60cae0690e115_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_typesqualifierstype_qualifie_Example_get_a_ptr@
hs_bindgen_64d60cae0690e115 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
hs_bindgen_64d60cae0690e115 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_64d60cae0690e115_base

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

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_e2844224d896e170" hs_bindgen_e2844224d896e170_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_typesqualifierstype_qualifie_Example_get_b_ptr@
hs_bindgen_e2844224d896e170 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))
hs_bindgen_e2844224d896e170 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_e2844224d896e170_base

{-# NOINLINE b_ptr #-}

{-| __C declaration:__ @b@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:7:19@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
b_ptr :: Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
b_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e2844224d896e170

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_ab653695917fba40" hs_bindgen_ab653695917fba40_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_typesqualifierstype_qualifie_Example_get_c_ptr@
hs_bindgen_ab653695917fba40 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr (Ptr.Ptr FC.CInt))
hs_bindgen_ab653695917fba40 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_ab653695917fba40_base

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

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_8e02c92809ce6a69" hs_bindgen_8e02c92809ce6a69_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_typesqualifierstype_qualifie_Example_get_d_ptr@
hs_bindgen_8e02c92809ce6a69 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))
hs_bindgen_8e02c92809ce6a69 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_8e02c92809ce6a69_base

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
