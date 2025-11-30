-- | Standard library external binding specification
--
-- This /private/ module may only be used by "HsBindgen.BindingSpec".
--
-- Intended for qualified import.
--
-- > import HsBindgen.BindingSpec.Private.Stdlib qualified as Stdlib
--
-- The types for these bindings are defined in @HsBindgen.Runtime.Prelude@ in
-- the @hs-bindgen-runtime@ library, in the same order.
module HsBindgen.BindingSpec.Private.Stdlib (
    -- * Binding specification
    bindingSpec
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import HsBindgen.BindingSpec.Private.Common
import HsBindgen.BindingSpec.Private.V1 qualified as BindingSpec
import HsBindgen.Errors
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Binding specification
-------------------------------------------------------------------------------}

-- | All standard library bindings
--
-- These bindings include types defined in @base@ as well as
-- @hs-bindgen-runtime@
bindingSpec :: BindingSpec.UnresolvedBindingSpec
bindingSpec = BindingSpec.BindingSpec{..}
  where
    bindingSpecTarget :: BindingSpec.BindingSpecTarget
    bindingSpecTarget = BindingSpec.AnyTarget

    bindingSpecModule :: Hs.ModuleName
    bindingSpecModule = "HsBindgen.Runtime.Prelude"

    bindingSpecCTypes ::
      Map
        C.QualName
        [(Set HashIncludeArg, Omittable BindingSpec.CTypeSpec)]
    bindingSpecHsTypes :: Map Hs.Identifier BindingSpec.HsTypeSpec
    (bindingSpecCTypes, bindingSpecHsTypes) = aux [
        -- Integral types
        mkT "int8_t"         "Int8"     cD intI inttypesH
      , mkT "int16_t"        "Int16"    cD intI inttypesH
      , mkT "int32_t"        "Int32"    cD intI inttypesH
      , mkT "int64_t"        "Int64"    cD intI inttypesH
      , mkT "uint8_t"        "Word8"    cD intI inttypesH
      , mkT "uint16_t"       "Word16"   cD intI inttypesH
      , mkT "uint32_t"       "Word32"   cD intI inttypesH
      , mkT "uint64_t"       "Word64"   cD intI inttypesH
      , mkT "int_least8_t"   "Int8"     cD intI inttypesH
      , mkT "int_least16_t"  "Int16"    cD intI inttypesH
      , mkT "int_least32_t"  "Int32"    cD intI inttypesH
      , mkT "int_least64_t"  "Int64"    cD intI inttypesH
      , mkT "uint_least8_t"  "Word8"    cD intI inttypesH
      , mkT "uint_least16_t" "Word16"   cD intI inttypesH
      , mkT "uint_least32_t" "Word32"   cD intI inttypesH
      , mkT "uint_least64_t" "Word64"   cD intI inttypesH
      , mkT "int_fast8_t"    "Int8"     cD intI inttypesH
      , mkT "int_fast16_t"   "Int16"    cD intI inttypesH
      , mkT "int_fast32_t"   "Int32"    cD intI inttypesH
      , mkT "int_fast64_t"   "Int64"    cD intI inttypesH
      , mkT "uint_fast8_t"   "Word8"    cD intI inttypesH
      , mkT "uint_fast16_t"  "Word16"   cD intI inttypesH
      , mkT "uint_fast32_t"  "Word32"   cD intI inttypesH
      , mkT "uint_fast64_t"  "Word64"   cD intI inttypesH
      , mkT "intmax_t"       "CIntMax"  cD intI inttypesH
      , mkT "uintmax_t"      "CUIntMax" cD intI inttypesH
      , mkT "intptr_t"       "CIntPtr"  cD intI inttypesH
      , mkT "uintptr_t"      "CUIntPtr" cD intI inttypesH
        -- Floating types
      , mkT "fenv_t"    "CFenvT"    cO [] $ mkH ["fenv.h"]
      , mkT "fexcept_t" "CFexceptT" cO [] $ mkH ["fenv.h"]
        -- Mathematical types
      , mkT "div_t"     "CDivT"     cD divI $ mkH ["stdlib.h"]
      , mkT "ldiv_t"    "CLdivT"    cD divI $ mkH ["stdlib.h"]
      , mkT "lldiv_t"   "CLldivT"   cD divI $ mkH ["stdlib.h"]
      , mkT "imaxdiv_t" "CImaxdivT" cD divI $ mkH ["inttypes.h"]
        -- Standard definitions
      , mkT "size_t" "CSize" cD intI $ mkH [
            "signal.h"
          , "stddef.h"
          , "stdio.h"
          , "stdlib.h"
          , "string.h"
          , "time.h"
          , "uchar.h"
          , "wchar.h"
          ]
      , mkT "ptrdiff_t" "CPtrdiff" cD intI $ mkH ["stddef.h"]
        -- Non-local jump types
      , mkT "jmp_buf" "CJmpBuf" cO [] $ mkH ["setjmp.h"]
        -- Wide character types
      , mkT "wchar_t" "CWchar" cD intI $ mkH [
            "inttypes.h"
          , "stddef.h"
          , "stdlib.h"
          , "wchar.h"
          ]
      , mkT "wint_t"    "CWintT"    cD intI $ mkH ["wchar.h", "wctype.h"]
      , mkT "mbstate_t" "CMbstateT" cO []   $ mkH ["uchar.h", "wchar.h"]
      , mkT "wctrans_t" "CWctransT" cD eqI  $ mkH ["wctype.h"]
      , mkT "wctype_t"  "CWctypeT"  cD eqI  $ mkH ["wchar.h", "wctype.h"]
      , mkT "char16_t"  "CChar16T"  cD intI $ mkH ["uchar.h"]
      , mkT "char32_t"  "CChar32T"  cD intI $ mkH ["uchar.h"]
        -- Time types
      , mkT "time_t"    "CTime"  cD timeI $ mkH ["signal.h", "time.h"]
      , mkT "clock_t"   "CClock" cD timeI $ mkH ["signal.h", "time.h"]
      , mkT "struct tm" "CTm"    cD eqI   $ mkH ["time.h"]
        -- File types
      , mkT "FILE"   "CFile" cO [] $ mkH ["stdio.h", "wchar.h"]
      , mkT "fpos_t" "CFpos" cO [] $ mkH ["stdio.h"]
        -- Signal types
      , mkT "sig_atomic_t" "CSigAtomic" cD intI $ mkH ["signal.h"]
      ]

    cD, cO :: BindingSpec.CTypeRep
    cD = BindingSpec.CTypeRepDefault
    cO = BindingSpec.CTypeRepOpaque

    inttypesH :: Set HashIncludeArg
    inttypesH = mkH ["inttypes.h", "stdint.h"]

    divI :: [Hs.TypeClass]
    divI = [Hs.Eq, Hs.Ord, Hs.ReadRaw, Hs.Show]

    eqI :: [Hs.TypeClass]
    eqI = [Hs.Eq, Hs.ReadRaw, Hs.Show, Hs.StaticSize, Hs.Storable, Hs.WriteRaw]

    intI :: [Hs.TypeClass]
    intI = [
        Hs.Bits
      , Hs.Bounded
      , Hs.Enum
      , Hs.Eq
      , Hs.FiniteBits
      , Hs.Integral
      , Hs.Ix
      , Hs.Num
      , Hs.Ord
      , Hs.Read
      , Hs.ReadRaw
      , Hs.Real
      , Hs.Show
      , Hs.StaticSize
      , Hs.Storable
      , Hs.WriteRaw
      ]

    timeI :: [Hs.TypeClass]
    timeI = [
        Hs.Enum
      , Hs.Eq
      , Hs.Num
      , Hs.Ord
      , Hs.Read
      , Hs.ReadRaw
      , Hs.Real
      , Hs.Show
      , Hs.StaticSize
      , Hs.Storable
      , Hs.WriteRaw
      ]

    aux ::
         [ ( ( C.QualName
             , [(Set HashIncludeArg, Omittable BindingSpec.CTypeSpec)]
             )
           , (Hs.Identifier, BindingSpec.HsTypeSpec)
           )
         ]
      -> ( Map
             C.QualName
             [(Set HashIncludeArg, Omittable BindingSpec.CTypeSpec)]
         , Map Hs.Identifier BindingSpec.HsTypeSpec
         )
    aux = bimap Map.fromList Map.fromList . unzip

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

mkH :: [FilePath] -> Set HashIncludeArg
mkH = Set.fromList . map HashIncludeArg

mkT ::
     Text
  -> Hs.Identifier
  -> BindingSpec.CTypeRep
  -> [Hs.TypeClass]
  -> Set HashIncludeArg
  -> ( ( C.QualName
       , [(Set HashIncludeArg, Omittable BindingSpec.CTypeSpec)]
       )
     , (Hs.Identifier, BindingSpec.HsTypeSpec)
     )
mkT t hsIdentifier cTypeRep insts headers = case C.parseQualName t of
    Nothing -> panicPure $ "invalid qualified name: " ++ show t
    Just cQualName ->
      ( (cQualName, [(headers, Require cTypeSpec)])
      , (hsIdentifier, hsTypeSpec)
      )
  where
    cTypeSpec :: BindingSpec.CTypeSpec
    cTypeSpec = BindingSpec.CTypeSpec {
        cTypeSpecIdentifier = Just hsIdentifier
      , cTypeSpecRep        = Just cTypeRep
      }

    hsTypeSpec :: BindingSpec.HsTypeSpec
    hsTypeSpec = BindingSpec.HsTypeSpec {
        hsTypeSpecInstances = Map.fromList [
            (inst, Require def)
          | inst <- insts
          ]
      }
