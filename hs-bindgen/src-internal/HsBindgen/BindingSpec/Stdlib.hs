-- | Standard library external binding specification
--
-- Intended for qualified import.
--
-- > import HsBindgen.BindingSpec.Stdlib qualified as Stdlib
--
-- The types for these bindings are defined in @HsBindgen.Runtime.Prelude@ in
-- the @hs-bindgen-runtime@ library, in the same order.
module HsBindgen.BindingSpec.Stdlib (
    -- * Bindings
    bindings
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.Paths
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell

{-------------------------------------------------------------------------------
  Bindings
-------------------------------------------------------------------------------}

-- | All standard library bindings
--
-- This includes 'baseBindings' and 'runtimeBindings'.
bindings :: BindingSpec.UnresolvedBindingSpec
bindings = BindingSpec.BindingSpec{bindingSpecTypes}
  where
    bindingSpecTypes ::
      Map
        C.QualName
        [(Set CHeaderIncludePath, BindingSpec.Omittable BindingSpec.TypeSpec)]
    bindingSpecTypes = Map.fromList [
        -- Integral types
        mkT "int8_t"         "Int8"     intI inttypesH
      , mkT "int16_t"        "Int16"    intI inttypesH
      , mkT "int32_t"        "Int32"    intI inttypesH
      , mkT "int64_t"        "Int64"    intI inttypesH
      , mkT "uint8_t"        "Word8"    intI inttypesH
      , mkT "uint16_t"       "Word16"   intI inttypesH
      , mkT "uint32_t"       "Word32"   intI inttypesH
      , mkT "uint64_t"       "Word64"   intI inttypesH
      , mkT "int_least8_t"   "Int8"     intI inttypesH
      , mkT "int_least16_t"  "Int16"    intI inttypesH
      , mkT "int_least32_t"  "Int32"    intI inttypesH
      , mkT "int_least64_t"  "Int64"    intI inttypesH
      , mkT "uint_least8_t"  "Word8"    intI inttypesH
      , mkT "uint_least16_t" "Word16"   intI inttypesH
      , mkT "uint_least32_t" "Word32"   intI inttypesH
      , mkT "uint_least64_t" "Word64"   intI inttypesH
      , mkT "int_fast8_t"    "Int8"     intI inttypesH
      , mkT "int_fast16_t"   "Int16"    intI inttypesH
      , mkT "int_fast32_t"   "Int32"    intI inttypesH
      , mkT "int_fast64_t"   "Int64"    intI inttypesH
      , mkT "uint_fast8_t"   "Word8"    intI inttypesH
      , mkT "uint_fast16_t"  "Word16"   intI inttypesH
      , mkT "uint_fast32_t"  "Word32"   intI inttypesH
      , mkT "uint_fast64_t"  "Word64"   intI inttypesH
      , mkT "intmax_t"       "CIntMax"  intI inttypesH
      , mkT "uintmax_t"      "CUIntMax" intI inttypesH
      , mkT "intptr_t"       "CIntPtr"  intI inttypesH
      , mkT "uintptr_t"      "CUIntPtr" intI inttypesH
        -- Floating types
      , mkT "fenv_t"    "CFenvT"    [] $ mkH ["fenv.h"]
      , mkT "fexcept_t" "CFexceptT" [] $ mkH ["fenv.h"]
        -- Mathematical types
      , mkT "div_t"     "CDivT"     divI $ mkH ["stdlib.h"]
      , mkT "ldiv_t"    "CLdivT"    divI $ mkH ["stdlib.h"]
      , mkT "lldiv_t"   "CLldivT"   divI $ mkH ["stdlib.h"]
      , mkT "imaxdiv_t" "CImaxdivT" divI $ mkH ["inttypes.h"]
        -- Standard definitions
      , mkT "size_t" "CSize" intI $ mkH [
            "signal.h"
          , "stddef.h"
          , "stdio.h"
          , "stdlib.h"
          , "string.h"
          , "time.h"
          , "uchar.h"
          , "wchar.h"
          ]
      , mkT "ptrdiff_t" "CPtrdiff" intI $ mkH ["stddef.h"]
        -- Non-local jump types
      , mkT "jmp_buf" "CJmpBuf" [] $ mkH ["setjmp.h"]
        -- Wide character types
      , mkT "wchar_t" "CWchar" intI $ mkH [
            "inttypes.h"
          , "stddef.h"
          , "stdlib.h"
          , "wchar.h"
          ]
      , mkT "wint_t"    "CWintT"    intI $ mkH ["wchar.h", "wctype.h"]
      , mkT "mbstate_t" "CMbstateT" []   $ mkH ["uchar.h", "wchar.h"]
      , mkT "wctrans_t" "CWctransT" eqI  $ mkH ["wctype.h"]
      , mkT "wctype_t"  "CWctypeT"  eqI  $ mkH ["wchar.h", "wctype.h"]
      , mkT "char16_t"  "CChar16T"  intI $ mkH ["uchar.h"]
      , mkT "char32_t"  "CChar32T"  intI $ mkH ["uchar.h"]
        -- Time types
      , mkT "time_t"    "CTime"  timeI $ mkH ["signal.h", "time.h"]
      , mkT "clock_t"   "CClock" timeI $ mkH ["signal.h", "time.h"]
      , mkT "struct tm" "CTm"    eqI   $ mkH ["time.h"]
        -- File types
      , mkT "FILE"   "CFile" [] $ mkH ["stdio.h", "wchar.h"]
      , mkT "fpos_t" "CFpos" [] $ mkH ["stdio.h"]
        -- Signal types
      , mkT "sig_atomic_t" "CSigAtomic" intI $ mkH ["signal.h"]
      ]

    inttypesH :: Set CHeaderIncludePath
    inttypesH = mkH ["inttypes.h", "stdint.h"]

    divI :: [HsTypeClass]
    divI = [Eq, Ord, ReadRaw, Show]

    eqI :: [HsTypeClass]
    eqI = [Eq, ReadRaw, Show, StaticSize, Storable, WriteRaw]

    intI :: [HsTypeClass]
    intI = [
        Bits
      , Bounded
      , Enum
      , Eq
      , FiniteBits
      , Integral
      , Ix
      , Num
      , Ord
      , Read
      , ReadRaw
      , Real
      , Show
      , StaticSize
      , Storable
      , WriteRaw
      ]

    timeI :: [HsTypeClass]
    timeI = [
         Enum
      ,  Eq
      ,  Num
      ,  Ord
      ,  Read
      ,  ReadRaw
      ,  Real
      ,  Show
      ,  StaticSize
      ,  Storable
      ,  WriteRaw
      ]

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

mkH :: [FilePath] -> Set CHeaderIncludePath
mkH = Set.fromList . map CHeaderSystemIncludePath

mkT ::
     Text
  -> HsIdentifier
  -> [HsTypeClass]
  -> Set CHeaderIncludePath
  -> ( C.QualName
     , [(Set CHeaderIncludePath , BindingSpec.Omittable BindingSpec.TypeSpec)]
     )
mkT t hsId insts headers = case C.parseQualName t of
    Just cQualName -> (cQualName, [(headers, BindingSpec.Require typeSpec)])
    Nothing -> panicPure $ "invalid qualified name: " ++ show t
  where
    typeSpec :: BindingSpec.TypeSpec
    typeSpec = BindingSpec.TypeSpec {
        typeSpecModule     = Just "HsBindgen.Runtime.Prelude"
      , typeSpecIdentifier = Just hsId
      , typeSpecInstances  = Map.fromList [
            (inst, BindingSpec.Require instanceSpec)
          | inst <- insts
          ]
      }

    instanceSpec :: BindingSpec.InstanceSpec
    instanceSpec = BindingSpec.InstanceSpec {
        instanceSpecStrategy    = Nothing
      , instanceSpecConstraints = []
      }
