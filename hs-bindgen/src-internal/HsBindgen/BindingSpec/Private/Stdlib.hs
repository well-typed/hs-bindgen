-- | Standard library external binding specification
--
-- This /private/ module may only be used by "HsBindgen.BindingSpec".
--
-- Intended for qualified import.
--
-- > import HsBindgen.BindingSpec.Private.Stdlib qualified as Stdlib
--
-- If you change this file, note that:
--
-- - The types for these bindings are defined in @HsBindgen.Runtime.Prelude@ in
--   the @hs-bindgen-runtime@ library, in the same order.
--
-- - The types for these bindings are tested in
--   @standard_library_external_binding_specs.h@ in the same order.
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

    -- Note that the \"least\" and \"fast\" types (such as @int_least32_t@)
    -- /cannot/ be defined in the standard library because their implementations
    -- differ across different @libc@ implementations, and users may choose
    -- which @libc@ to use when running @hs-bindgen@.
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
                "tm_sec"
              , "tm_min"
              , "tm_hour"
              , "tm_mday"
              , "tm_mon"
              , "tm_year"
              , "tm_wday"
              , "tm_yday"
              , "tm_isdst"
              ]
        in  mkType "struct tm" "CTm" hsR tmI ["time.h"]
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

    intI, nEqI, timeI, tmI :: [Inst.TypeClass]
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
    tmI = [ -- struct tm
        Inst.Eq
      , Inst.HasCField
      , Inst.HasField
      , Inst.ReadRaw
      , Inst.Show
      , Inst.StaticSize
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
