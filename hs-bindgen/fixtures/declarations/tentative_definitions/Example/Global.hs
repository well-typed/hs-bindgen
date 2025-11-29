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
  , "/* ExampleNothingget_i1_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_declarationstentative_definit_8a4a155fb4b3e983 (void)"
  , "{"
  , "  return &i1;"
  , "}"
  , "/* ExampleNothingget_i2_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_declarationstentative_definit_8a341976b53c3159 (void)"
  , "{"
  , "  return &i2;"
  , "}"
  , "/* ExampleNothingget_i3_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_declarationstentative_definit_8a18e8a325536dc5 (void)"
  , "{"
  , "  return &i3;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_i1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_declarationstentative_definit_8a4a155fb4b3e983" hs_bindgen_test_declarationstentative_definit_8a4a155fb4b3e983 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i1_ptr #-}

{-| __C declaration:__ @i1@

    __defined at:__ @declarations\/tentative_definitions.h:17:5@

    __exported by:__ @declarations\/tentative_definitions.h@
-}
i1_ptr :: Ptr.Ptr FC.CInt
i1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_declarationstentative_definit_8a4a155fb4b3e983

{-| __unique:__ @ExampleNothingget_i2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_declarationstentative_definit_8a341976b53c3159" hs_bindgen_test_declarationstentative_definit_8a341976b53c3159 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i2_ptr #-}

{-| __C declaration:__ @i2@

    __defined at:__ @declarations\/tentative_definitions.h:21:12@

    __exported by:__ @declarations\/tentative_definitions.h@
-}
i2_ptr :: Ptr.Ptr FC.CInt
i2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_declarationstentative_definit_8a341976b53c3159

{-| __unique:__ @ExampleNothingget_i3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_declarationstentative_definit_8a18e8a325536dc5" hs_bindgen_test_declarationstentative_definit_8a18e8a325536dc5 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i3_ptr #-}

{-| __C declaration:__ @i3@

    __defined at:__ @declarations\/tentative_definitions.h:30:5@

    __exported by:__ @declarations\/tentative_definitions.h@
-}
i3_ptr :: Ptr.Ptr FC.CInt
i3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_declarationstentative_definit_8a18e8a325536dc5
