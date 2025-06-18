-- | Standard library external binding specification
--
-- Intended for qualified import.
--
-- > import HsBindgen.BindingSpec.Stdlib qualified as Stdlib
module HsBindgen.BindingSpec.Stdlib (
    -- * Bindings
    bindings
  , baseBindings
  , runtimeBindings
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.Paths
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Imports
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
        BindingSpec.CSpelling
        [(Set CHeaderIncludePath, BindingSpec.Omittable BindingSpec.TypeSpec)]
    bindingSpecTypes =
      BindingSpec.bindingSpecTypes baseBindings
        <> BindingSpec.bindingSpecTypes runtimeBindings

-- | External bindings from @base@
baseBindings :: BindingSpec.UnresolvedBindingSpec
baseBindings = BindingSpec.BindingSpec{bindingSpecTypes}
  where
    bindingSpecTypes ::
      Map
        BindingSpec.CSpelling
        [(Set CHeaderIncludePath, BindingSpec.Omittable BindingSpec.TypeSpec)]
    bindingSpecTypes = Map.fromList [
        -- Integral types
        mkT "int8_t"         "Int8"     "Data.Int"        intI inttypesH
      , mkT "int16_t"        "Int16"    "Data.Int"        intI inttypesH
      , mkT "int32_t"        "Int32"    "Data.Int"        intI inttypesH
      , mkT "int64_t"        "Int64"    "Data.Int"        intI inttypesH
      , mkT "uint8_t"        "Word8"    "Data.Word"       intI inttypesH
      , mkT "uint16_t"       "Word16"   "Data.Word"       intI inttypesH
      , mkT "uint32_t"       "Word32"   "Data.Word"       intI inttypesH
      , mkT "uint64_t"       "Word64"   "Data.Word"       intI inttypesH
      , mkT "int_least8_t"   "Int8"     "Data.Int"        intI inttypesH
      , mkT "int_least16_t"  "Int16"    "Data.Int"        intI inttypesH
      , mkT "int_least32_t"  "Int32"    "Data.Int"        intI inttypesH
      , mkT "int_least64_t"  "Int64"    "Data.Int"        intI inttypesH
      , mkT "uint_least8_t"  "Word8"    "Data.Word"       intI inttypesH
      , mkT "uint_least16_t" "Word16"   "Data.Word"       intI inttypesH
      , mkT "uint_least32_t" "Word32"   "Data.Word"       intI inttypesH
      , mkT "uint_least64_t" "Word64"   "Data.Word"       intI inttypesH
      , mkT "int_fast8_t"    "Int8"     "Data.Int"        intI inttypesH
      , mkT "int_fast16_t"   "Int16"    "Data.Int"        intI inttypesH
      , mkT "int_fast32_t"   "Int32"    "Data.Int"        intI inttypesH
      , mkT "int_fast64_t"   "Int64"    "Data.Int"        intI inttypesH
      , mkT "uint_fast8_t"   "Word8"    "Data.Word"       intI inttypesH
      , mkT "uint_fast16_t"  "Word16"   "Data.Word"       intI inttypesH
      , mkT "uint_fast32_t"  "Word32"   "Data.Word"       intI inttypesH
      , mkT "uint_fast64_t"  "Word64"   "Data.Word"       intI inttypesH
      , mkT "intmax_t"       "CIntMax"  "Foreign.C.Types" intI inttypesH
      , mkT "uintmax_t"      "CUIntMax" "Foreign.C.Types" intI inttypesH
      , mkT "intptr_t"       "CIntPtr"  "Foreign.C.Types" intI inttypesH
      , mkT "uintptr_t"      "CUIntPtr" "Foreign.C.Types" intI inttypesH
        -- Standard definitions
      , mkT "size_t" "CSize" "Foreign.C.Types" intI $ mkH [
            "signal.h"
          , "stddef.h"
          , "stdio.h"
          , "stdlib.h"
          , "string.h"
          , "time.h"
          , "uchar.h"
          , "wchar.h"
          ]
      , mkT "ptrdiff_t" "CPtrdiff" "Foreign.C.Types" intI $ mkH ["stddef.h"]
        -- Non-local jump types
      , mkT "jmp_buf" "CJmpBuf" "Foreign.C.Types" [] $ mkH ["setjmp.h"]
        -- Wide character types
      , mkT "wchar_t" "CWchar" "Foreign.C.Types" intI $ mkH [
            "inttypes.h"
          , "stddef.h"
          , "stdlib.h"
          , "wchar.h"
          ]
        -- Time types
      , mkT "time_t"  "CTime"  "Foreign.C.Types" timeI $
          mkH ["signal.h", "time.h"]
      , mkT "clock_t" "CClock" "Foreign.C.Types" timeI $
          mkH ["signal.h", "time.h"]
        -- File types
      , mkT "FILE"   "CFile" "Foreign.C.Types" [] $
          mkH ["stdio.h", "wchar.h"]
      , mkT "fpos_t" "CFpos" "Foreign.C.Types" [] $ mkH ["stdio.h"]
        -- Signal types
      , mkT "sig_atomic_t" "CSigAtomic" "Foreign.C.Types" intI $
          mkH ["signal.h"]
      ]

    inttypesH :: Set CHeaderIncludePath
    inttypesH = mkH ["inttypes.h", "stdint.h"]

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

-- | External bindings from @hs-bindgen-runtime@
runtimeBindings :: BindingSpec.UnresolvedBindingSpec
runtimeBindings = BindingSpec.BindingSpec{bindingSpecTypes}
  where
    bindingSpecTypes ::
      Map
        BindingSpec.CSpelling
        [(Set CHeaderIncludePath, BindingSpec.Omittable BindingSpec.TypeSpec)]
    bindingSpecTypes = Map.fromList [
        -- Floating types
        mkT "fenv_t"       "CFenvT"    hsMod [] $ mkH ["fenv.h"]
      , mkT "fexcept_t"    "CFexceptT" hsMod [] $ mkH ["fenv.h"]
        -- Mathematical types
      , mkT "div_t"        "CDivT"     hsMod divI $ mkH ["stdlib.h"]
      , mkT "ldiv_t"       "CLdivT"    hsMod divI $ mkH ["stdlib.h"]
      , mkT "lldiv_t"      "CLldivT"   hsMod divI $ mkH ["stdlib.h"]
      , mkT "imaxdiv_t"    "CImaxdivT" hsMod divI $ mkH ["inttypes.h"]
        -- Wide character types
      , mkT "wint_t"       "CWintT"    hsMod intI $ mkH ["wchar.h", "wctype.h"]
      , mkT "mbstate_t"    "CMbstateT" hsMod []   $ mkH ["uchar.h", "wchar.h"]
      , mkT "wctrans_t"    "CWctransT" hsMod eqI  $ mkH ["wctype.h"]
      , mkT "wctype_t"     "CWctypeT"  hsMod eqI  $ mkH ["wchar.h", "wctype.h"]
      , mkT "char16_t"     "CChar16T"  hsMod intI $ mkH ["uchar.h"]
      , mkT "char32_t"     "CChar32T"  hsMod intI $ mkH ["uchar.h"]
        -- Time types
      , mkT "struct tm"    "CTm"       hsMod eqI  $ mkH ["time.h"]
      ]

    hsMod :: HsModuleName
    hsMod = "HsBindgen.Runtime.LibC"

    divI :: [HsTypeClass]
    divI = [Eq, Ord, ReadRaw, Show]

    eqI :: [HsTypeClass]
    eqI = [Eq, ReadRaw, Show, StaticSize, Storable, WriteRaw]

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

mkH :: [FilePath] -> Set CHeaderIncludePath
mkH = Set.fromList . map CHeaderSystemIncludePath

mkT ::
     BindingSpec.CSpelling
  -> HsIdentifier
  -> HsModuleName
  -> [HsTypeClass]
  -> Set CHeaderIncludePath
  -> ( BindingSpec.CSpelling
     , [(Set CHeaderIncludePath , BindingSpec.Omittable BindingSpec.TypeSpec)]
     )
mkT spelling hsId hsMod insts headers =
    (spelling,) . pure . (headers,) $ BindingSpec.Require typeSpec
  where
    typeSpec :: BindingSpec.TypeSpec
    typeSpec = BindingSpec.TypeSpec {
        typeSpecModule     = Just hsMod
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
