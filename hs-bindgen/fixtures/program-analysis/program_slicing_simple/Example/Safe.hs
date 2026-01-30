{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign
import qualified Foreign.C as FC
import qualified GHC.Int
import qualified GHC.Word
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <program-analysis/program_slicing_simple.h>"
  , "signed int hs_bindgen_48dbbf4b09b5b3c1 ("
  , "  uint64_t arg1,"
  , "  uint32_t arg2"
  , ")"
  , "{"
  , "  return bar(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_programanalysisprogram_slici_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_48dbbf4b09b5b3c1" hs_bindgen_48dbbf4b09b5b3c1_base ::
     GHC.Word.Word64
  -> GHC.Word.Word32
  -> IO GHC.Int.Int32

-- __unique:__ @test_programanalysisprogram_slici_Example_Safe_bar@
hs_bindgen_48dbbf4b09b5b3c1 ::
     Foreign.Word64
  -> Uint32_t
  -> IO FC.CInt
hs_bindgen_48dbbf4b09b5b3c1 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_48dbbf4b09b5b3c1_base

{-| __C declaration:__ @bar@

    __defined at:__ @program-analysis\/program_slicing_simple.h 8:5@

    __exported by:__ @program-analysis\/program_slicing_simple.h@
-}
bar ::
     Foreign.Word64
     -- ^ __C declaration:__ @x@
  -> Uint32_t
     -- ^ __C declaration:__ @y@
  -> IO FC.CInt
bar = hs_bindgen_48dbbf4b09b5b3c1
