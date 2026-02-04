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
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Binding specification
-------------------------------------------------------------------------------}

-- | All standard library bindings
--
-- These bindings include types defined in @base@ as well as
-- @hs-bindgen-runtime@
bindingSpec :: BindingSpec.UnresolvedBindingSpec
bindingSpec = BindingSpec.BindingSpec{
      moduleName = "HsBindgen.Runtime.LibC"
    , cTypes     = bindingSpecCTypes
    , hsTypes    = bindingSpecHsTypes
    }
  where
    bindingSpecCTypes  :: CTypeMap
    bindingSpecHsTypes :: HsTypeMap
    (bindingSpecCTypes, bindingSpecHsTypes) = mkMaps $
         boolTypes
      ++ integralTypes
      ++ floatingTypes
      ++ mathTypes
      ++ stdTypes
      ++ nonLocalJumpTypes
      ++ wcharTypes
      ++ timeTypes
      ++ fileTypes
      ++ signalTypes

    boolTypes :: [(CTypeKV, HsTypeKV)]
    boolTypes = [
        mkTypeN "bool" "CBool" intI ["stdbool.h"]
      ]

    integralTypes :: [(CTypeKV, HsTypeKV)]
    integralTypes =
      let aux (t, hsIdentifier) =
            mkTypeN t hsIdentifier intI  ["inttypes.h", "stdint.h"]
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
      let aux (t, hsIdentifier) = mkType t hsIdentifier hsED [] ["fenv.h"]
      in  map aux [
              ("fenv_t",    "CFenvT")
            , ("fexcept_t", "CFexceptT")
            ]

    mathTypes :: [(CTypeKV, HsTypeKV)]
    mathTypes = [
        let hsR = mkHsR "CDivT" ["cDivT_quot", "cDivT_rem"]
        in  mkType "div_t"     "CDivT"     hsR divI ["stdlib.h"]
      , let hsR = mkHsR "CLdivT" ["cLdivT_quot", "cLdivT_rem"]
        in  mkType "ldiv_t"    "CLdivT"    hsR divI ["stdlib.h"]
      , let hsR = mkHsR "CLldivT" ["cLldivT_quot", "cLldivT_rem"]
        in  mkType "lldiv_t"   "CLldivT"   hsR divI ["stdlib.h"]
      , let hsR = mkHsR "CImaxdivT" ["cImaxdivT_quot", "cImaxdivT_rem"]
        in  mkType "imaxdiv_t" "CImaxdivT" hsR divI ["inttypes.h"]
      ]

    stdTypes :: [(CTypeKV, HsTypeKV)]
    stdTypes = [
        mkTypeN "size_t" "CSize" intI [
            "signal.h"
          , "stddef.h"
          , "stdio.h"
          , "stdlib.h"
          , "string.h"
          , "time.h"
          , "uchar.h"
          , "wchar.h"
          ]
      , mkTypeN "ptrdiff_t" "CPtrdiff" intI ["stddef.h"]
      ]

    nonLocalJumpTypes :: [(CTypeKV, HsTypeKV)]
    nonLocalJumpTypes = [
        mkType "jmp_buf" "CJmpBuf" hsED [] ["setjmp.h"]
      ]

    wcharTypes :: [(CTypeKV, HsTypeKV)]
    wcharTypes = [
        mkTypeN "wchar_t" "CWchar" intI [
            "inttypes.h"
          , "stddef.h"
          , "stdlib.h"
          , "wchar.h"
          ]
      , mkTypeN "wint_t"    "CWintT"         intI ["wchar.h", "wctype.h"]
      , mkType  "mbstate_t" "CMbstateT" hsED []   ["uchar.h", "wchar.h"]
      , mkTypeN "wctrans_t" "CWctransT"      nEqI ["wctype.h"]
      , mkTypeN "wctype_t"  "CWctypeT"       nEqI ["wchar.h", "wctype.h"]
      , mkTypeN "char16_t"  "CChar16T"       intI ["uchar.h"]
      , mkTypeN "char32_t"  "CChar32T"       intI ["uchar.h"]
      ]

    timeTypes :: [(CTypeKV, HsTypeKV)]
    timeTypes = [
        mkTypeN "time_t"  "CTime"  timeI ["signal.h", "time.h"]
      , mkTypeN "clock_t" "CClock" timeI ["signal.h", "time.h"]
      , let hsR = mkHsR "CTm" [
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
        in  mkType "struct tm" "CTm" hsR rEqI ["time.h"]
      ]

    fileTypes :: [(CTypeKV, HsTypeKV)]
    fileTypes = [
        mkType "FILE"   "CFile" hsED [] ["stdio.h", "wchar.h"]
      , mkType "fpos_t" "CFpos" hsED [] ["stdio.h"]
      ]

    signalTypes :: [(CTypeKV, HsTypeKV)]
    signalTypes = [
        mkTypeN "sig_atomic_t" "CSigAtomic" intI ["signal.h"]
      ]

    divI, intI, nEqI, rEqI, timeI :: [Inst.TypeClass]
    divI = [
        Inst.Eq
      , Inst.HasCField
      , Inst.HasField
      , Inst.Ord
      , Inst.ReadRaw
      , Inst.Show
      , Inst.StaticSize
      ]
    intI = [
        Inst.Bitfield
      , Inst.Bits
      , Inst.Bounded
      , Inst.Enum
      , Inst.Eq
      , Inst.FiniteBits
      , Inst.HasFFIType
      , Inst.Integral
      , Inst.Ix
      , Inst.Num
      , Inst.Ord
      , Inst.Prim
      , Inst.Read
      , Inst.ReadRaw
      , Inst.Real
      , Inst.Show
      , Inst.StaticSize
      , Inst.Storable
      , Inst.WriteRaw
      ]
    nEqI = [ -- newtype equality
        Inst.Eq
      , Inst.HasFFIType
      , Inst.Prim
      , Inst.ReadRaw
      , Inst.Show
      , Inst.StaticSize
      , Inst.Storable
      , Inst.WriteRaw
      ]
    rEqI = [ -- record equality
        Inst.Eq
      , Inst.HasCField
      , Inst.HasField
      , Inst.ReadRaw
      , Inst.Show
      , Inst.StaticSize
      , Inst.Storable
      , Inst.WriteRaw
      ]
    timeI = [
        Inst.Enum
      , Inst.Eq
      , Inst.HasFFIType
      , Inst.Num
      , Inst.Ord
      , Inst.Read
      , Inst.ReadRaw
      , Inst.Real
      , Inst.Show
      , Inst.StaticSize
      , Inst.Storable
      , Inst.WriteRaw
      ]

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- | Concise alias for the C type 'Map'
type CTypeMap =
  Map DeclId [(Set HashIncludeArg, Omittable BindingSpec.CTypeSpec)]

-- | Concise alias for the key and value tuple corresponding to an entry in a
-- 'CTypeMap'
type CTypeKV =
  (DeclId, [(Set HashIncludeArg, Omittable BindingSpec.CTypeSpec)])

-- | Concise alias for the Haskell type 'Map'
type HsTypeMap = Map Hs.Identifier BindingSpec.HsTypeSpec

-- | Concise alias for the key and value tuple corresponding to an entry in a
-- 'HsTypeMap'
type HsTypeKV = (Hs.Identifier, BindingSpec.HsTypeSpec)

mkMaps :: [(CTypeKV, HsTypeKV)] -> (CTypeMap, HsTypeMap)
mkMaps = bimap Map.fromList Map.fromList . unzip

-- | Construct the 'CTypeKV' and 'HsTypeKV' for a type
mkType ::
     Text
  -> Hs.Identifier
  -> BindingSpec.HsTypeRep
  -> [Inst.TypeClass]
  -> [FilePath]
  -> (CTypeKV, HsTypeKV)
mkType t hsIdentifier hsTypeRep insts headers' =
    case parseDeclId t of
      Just cDeclId ->
        ( (cDeclId, [(headers, Require cTypeSpec)])
        , (hsIdentifier, hsTypeSpec)
        )
      Nothing -> panicPure $ "invalid declaration ID: " ++ show t
  where
    headers :: Set HashIncludeArg
    headers = Set.fromList $ map HashIncludeArg headers'

    cTypeSpec :: BindingSpec.CTypeSpec
    cTypeSpec = BindingSpec.CTypeSpec {
        hsIdent = Just hsIdentifier
      }

    hsTypeSpec :: BindingSpec.HsTypeSpec
    hsTypeSpec = BindingSpec.HsTypeSpec {
        hsRep     = Just hsTypeRep
      , instances = Map.fromList [
            (inst, Require def)
          | inst <- insts
          ]
      }

-- | Concise alias for 'BindingSpec.HsTypeRepEmptyData'
hsED :: BindingSpec.HsTypeRep
hsED = BindingSpec.HsTypeRepEmptyData

-- | Construct a 'BindingSpec.HsTypeRepRecord' with the specified constructor
-- and field names
mkHsR :: Hs.Identifier -> [Hs.Identifier] -> BindingSpec.HsTypeRep
mkHsR constructorName fieldNames = BindingSpec.HsTypeRepRecord $
    BindingSpec.HsRecordRep {
        constructor = Just constructorName
      , fields      = Just fieldNames
      }

-- | Construct a 'BindingSpec.HsTypeRepNewtype' with the specified constructor
-- name and no field names
--
-- The standard @newtype@ types do not have field names.
mkHsN :: Hs.Identifier -> BindingSpec.HsTypeRep
mkHsN constructorName = BindingSpec.HsTypeRepNewtype $
    BindingSpec.HsNewtypeRep {
        constructor = Just constructorName
      , field       = Nothing
      }

-- | Variant of 'mkType' that creates a 'BindingSpec.HsTypeRepNewtype' where the
-- constructor has the same name as the type
mkTypeN ::
     Text
  -> Hs.Identifier
  -> [Inst.TypeClass]
  -> [FilePath]
  -> (CTypeKV, HsTypeKV)
mkTypeN t hsIdentifier insts headers =
    mkType t hsIdentifier (mkHsN hsIdentifier) insts headers
