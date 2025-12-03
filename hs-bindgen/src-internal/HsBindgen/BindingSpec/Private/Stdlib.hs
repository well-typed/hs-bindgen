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

    bindingSpecCTypes  :: CTypeMap
    bindingSpecHsTypes :: HsTypeMap
    (bindingSpecCTypes, bindingSpecHsTypes) = mkMaps $
         integralTypes
      ++ floatingTypes
      ++ mathTypes
      ++ stdTypes
      ++ nonLocalJumpTypes
      ++ wcharTypes
      ++ timeTypes
      ++ fileTypes
      ++ signalTypes

    integralTypes :: [(CTypeKV, HsTypeKV)]
    integralTypes =
      let aux (t, hsIdentifier) =
            mkType t hsIdentifier cD hsN intI ["inttypes.h", "stdint.h"]
      in  map aux [
              ("int8_t",         "Int8")
            , ("int16_t",        "Int16")
            , ("int32_t",        "Int32")
            , ("int64_t",        "Int64")
            , ("uint8_t",        "Word8")
            , ("uint16_t",       "Word16")
            , ("uint32_t",       "Word32")
            , ("uint64_t",       "Word64")
            , ("int_least8_t",   "Int8")
            , ("int_least16_t",  "Int16")
            , ("int_least32_t",  "Int32")
            , ("int_least64_t",  "Int64")
            , ("uint_least8_t",  "Word8")
            , ("uint_least16_t", "Word16")
            , ("uint_least32_t", "Word32")
            , ("uint_least64_t", "Word64")
            , ("int_fast8_t",    "Int8")
            , ("int_fast16_t",   "Int16")
            , ("int_fast32_t",   "Int32")
            , ("int_fast64_t",   "Int64")
            , ("uint_fast8_t",   "Word8")
            , ("uint_fast16_t",  "Word16")
            , ("uint_fast32_t",  "Word32")
            , ("uint_fast64_t",  "Word64")
            , ("intmax_t",       "CIntMax")
            , ("uintmax_t",      "CUIntMax")
            , ("intptr_t",       "CIntPtr")
            , ("uintptr_t",      "CUIntPtr")
            ]

    floatingTypes :: [(CTypeKV, HsTypeKV)]
    floatingTypes =
      let aux (t, hsIdentifier) = mkType t hsIdentifier cO hsO [] ["fenv.h"]
      in  map aux [
              ("fenv_t",    "CFenvT")
            , ("fexcept_t", "CFexceptT")
            ]

    mathTypes :: [(CTypeKV, HsTypeKV)]
    mathTypes = [
        let hsR = mkHsR ["cDivT_quot", "cDivT_rem"]
        in  mkType "div_t"     "CDivT"     cD hsR divI ["stdlib.h"]
      , let hsR = mkHsR ["cLdivT_quot", "cLdivT_rem"]
        in  mkType "ldiv_t"    "CLdivT"    cD hsR divI ["stdlib.h"]
      , let hsR = mkHsR ["cLldivT_quot", "cLldivT_rem"]
        in  mkType "lldiv_t"   "CLldivT"   cD hsR divI ["stdlib.h"]
      , let hsR = mkHsR ["cImaxdivT_quot", "cImaxdivT_rem"]
        in  mkType "imaxdiv_t" "CImaxdivT" cD hsR divI ["inttypes.h"]
      ]

    stdTypes :: [(CTypeKV, HsTypeKV)]
    stdTypes = [
        mkType "size_t" "CSize" cD hsN intI [
            "signal.h"
          , "stddef.h"
          , "stdio.h"
          , "stdlib.h"
          , "string.h"
          , "time.h"
          , "uchar.h"
          , "wchar.h"
          ]
      , mkType "ptrdiff_t" "CPtrdiff" cD hsN intI ["stddef.h"]
      ]

    nonLocalJumpTypes :: [(CTypeKV, HsTypeKV)]
    nonLocalJumpTypes = [
        mkType "jmp_buf" "CJmpBuf" cO hsO [] ["setjmp.h"]
      ]

    wcharTypes :: [(CTypeKV, HsTypeKV)]
    wcharTypes = [
        mkType "wchar_t" "CWchar" cD hsN intI [
            "inttypes.h"
          , "stddef.h"
          , "stdlib.h"
          , "wchar.h"
          ]
      , mkType "wint_t"    "CWintT"    cD hsN intI ["wchar.h", "wctype.h"]
      , mkType "mbstate_t" "CMbstateT" cO hsO []   ["uchar.h", "wchar.h"]
      , mkType "wctrans_t" "CWctransT" cD hsN eqI  ["wctype.h"]
      , mkType "wctype_t"  "CWctypeT"  cD hsN eqI  ["wchar.h", "wctype.h"]
      , mkType "char16_t"  "CChar16T"  cD hsN intI ["uchar.h"]
      , mkType "char32_t"  "CChar32T"  cD hsN intI ["uchar.h"]
      ]

    timeTypes :: [(CTypeKV, HsTypeKV)]
    timeTypes = [
        mkType "time_t"  "CTime"  cD hsN timeI ["signal.h", "time.h"]
      , mkType "clock_t" "CClock" cD hsN timeI ["signal.h", "time.h"]
      , let hsR = mkHsR [
                "cTm_sec"
              , "cTm_min"
              , "cTm_hour"
              , "cTm_mday"
              , "cTm_mon"
              , "cTm_year"
              , "cTm_wday"
              , "cTm_yday"
              , "cTm_isdst"
              ]
        in  mkType "struct tm" "CTm" cD hsR eqI ["time.h"]
      ]

    fileTypes :: [(CTypeKV, HsTypeKV)]
    fileTypes = [
        mkType "FILE"   "CFile" cO hsO [] ["stdio.h", "wchar.h"]
      , mkType "fpos_t" "CFpos" cO hsO [] ["stdio.h"]
      ]

    signalTypes :: [(CTypeKV, HsTypeKV)]
    signalTypes = [
        mkType "sig_atomic_t" "CSigAtomic" cD hsN intI ["signal.h"]
      ]

    divI, eqI, intI, timeI :: [Hs.TypeClass]
    divI = [Hs.Eq, Hs.Ord, Hs.ReadRaw, Hs.Show]
    eqI = [Hs.Eq, Hs.ReadRaw, Hs.Show, Hs.StaticSize, Hs.Storable, Hs.WriteRaw]
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

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

type CTypeMap =
  Map C.QualName [(Set HashIncludeArg, Omittable BindingSpec.CTypeSpec)]

type CTypeKV =
  (C.QualName, [(Set HashIncludeArg, Omittable BindingSpec.CTypeSpec)])

type HsTypeMap = Map Hs.Identifier BindingSpec.HsTypeSpec

type HsTypeKV = (Hs.Identifier, BindingSpec.HsTypeSpec)

mkMaps :: [(CTypeKV, HsTypeKV)] -> (CTypeMap, HsTypeMap)
mkMaps = bimap Map.fromList Map.fromList . unzip

mkType ::
     Text
  -> Hs.Identifier
  -> BindingSpec.CTypeRep
  -> BindingSpec.HsTypeRep
  -> [Hs.TypeClass]
  -> [FilePath]
  -> (CTypeKV, HsTypeKV)
mkType t hsIdentifier cTypeRep hsTypeRep insts headers' =
    case C.parseQualName t of
      Just cQualName ->
        ( (cQualName, [(headers, Require cTypeSpec)])
        , (hsIdentifier, hsTypeSpec)
        )
      Nothing -> panicPure $ "invalid qualified name: " ++ show t
  where
    headers :: Set HashIncludeArg
    headers = Set.fromList $ map HashIncludeArg headers'

    cTypeSpec :: BindingSpec.CTypeSpec
    cTypeSpec = BindingSpec.CTypeSpec {
        cTypeSpecIdentifier = Just hsIdentifier
      , cTypeSpecRep        = Just cTypeRep
      }

    hsTypeSpec :: BindingSpec.HsTypeSpec
    hsTypeSpec = BindingSpec.HsTypeSpec {
        hsTypeSpecRep       = Just hsTypeRep
      , hsTypeSpecInstances = Map.fromList [
            (inst, Require def)
          | inst <- insts
          ]
      }

cD, cO :: BindingSpec.CTypeRep
cD = BindingSpec.CTypeRepDefault
cO = BindingSpec.CTypeRepOpaque

mkHsR :: [Hs.Identifier] -> BindingSpec.HsTypeRep
mkHsR = BindingSpec.HsTypeRepRecord . BindingSpec.HsRecordRep . Just

hsN, hsO :: BindingSpec.HsTypeRep
hsN = BindingSpec.HsTypeRepNewtype
hsO = BindingSpec.HsTypeRepOpaque
