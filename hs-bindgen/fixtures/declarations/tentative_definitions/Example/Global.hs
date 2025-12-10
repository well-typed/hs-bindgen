{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <declarations/tentative_definitions.h>"
  , "/* test_declarationstentative_definit_Example_get_i1_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_a6c085c30d844c6a (void)"
  , "{"
  , "  return &i1;"
  , "}"
  , "/* test_declarationstentative_definit_Example_get_i2_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_f63a7b78dc0eb182 (void)"
  , "{"
  , "  return &i2;"
  , "}"
  , "/* test_declarationstentative_definit_Example_get_i3_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_927fb780e7e73ba1 (void)"
  , "{"
  , "  return &i3;"
  , "}"
  ]))

-- | __unique:__ @test_declarationstentative_definit_Example_get_i1_ptr@
foreign import ccall unsafe "hs_bindgen_a6c085c30d844c6a" hs_bindgen_a6c085c30d844c6a ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i1_ptr #-}

{-| __C declaration:__ @i1@

    __defined at:__ @declarations\/tentative_definitions.h:17:5@

    __exported by:__ @declarations\/tentative_definitions.h@
-}
i1_ptr :: Ptr.Ptr FC.CInt
i1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a6c085c30d844c6a

-- | __unique:__ @test_declarationstentative_definit_Example_get_i2_ptr@
foreign import ccall unsafe "hs_bindgen_f63a7b78dc0eb182" hs_bindgen_f63a7b78dc0eb182 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i2_ptr #-}

{-| __C declaration:__ @i2@

    __defined at:__ @declarations\/tentative_definitions.h:21:12@

    __exported by:__ @declarations\/tentative_definitions.h@
-}
i2_ptr :: Ptr.Ptr FC.CInt
i2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f63a7b78dc0eb182

-- | __unique:__ @test_declarationstentative_definit_Example_get_i3_ptr@
foreign import ccall unsafe "hs_bindgen_927fb780e7e73ba1" hs_bindgen_927fb780e7e73ba1 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i3_ptr #-}

{-| __C declaration:__ @i3@

    __defined at:__ @declarations\/tentative_definitions.h:30:5@

    __exported by:__ @declarations\/tentative_definitions.h@
-}
i3_ptr :: Ptr.Ptr FC.CInt
i3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_927fb780e7e73ba1
