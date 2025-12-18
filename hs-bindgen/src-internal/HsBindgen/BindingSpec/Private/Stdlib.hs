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

import HsBindgen.Runtime.BaseForeignType qualified as BFT

import HsBindgen.BindingSpec.Private.Common
import HsBindgen.BindingSpec.Private.V1 qualified as BindingSpec
import HsBindgen.Errors
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
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
      let aux (t, hsIdentifier, ffitype) =
            mkTypeN t hsIdentifier cD intI (Just ffitype) ["inttypes.h", "stdint.h"]
      in  map aux [
              ("int8_t",         "Int8",     BindingSpec.Basic BFT.Int8)
            , ("int16_t",        "Int16",    BindingSpec.Basic BFT.Int16)
            , ("int32_t",        "Int32",    BindingSpec.Basic BFT.Int32)
            , ("int64_t",        "Int64",    BindingSpec.Basic BFT.Int64)
            , ("uint8_t",        "Word8",    BindingSpec.Basic BFT.Word8)
            , ("uint16_t",       "Word16",   BindingSpec.Basic BFT.Word16)
            , ("uint32_t",       "Word32",   BindingSpec.Basic BFT.Word32)
            , ("uint64_t",       "Word64",   BindingSpec.Basic BFT.Word64)
            , ("int_least8_t",   "Int8",     BindingSpec.Basic BFT.Int8)
            , ("int_least16_t",  "Int16",    BindingSpec.Basic BFT.Int16)
            , ("int_least32_t",  "Int32",    BindingSpec.Basic BFT.Int32)
            , ("int_least64_t",  "Int64",    BindingSpec.Basic BFT.Int64)
            , ("uint_least8_t",  "Word8",    BindingSpec.Basic BFT.Word8)
            , ("uint_least16_t", "Word16",   BindingSpec.Basic BFT.Word16)
            , ("uint_least32_t", "Word32",   BindingSpec.Basic BFT.Word32)
            , ("uint_least64_t", "Word64",   BindingSpec.Basic BFT.Word64)
            , ("int_fast8_t",    "Int8",     BindingSpec.Basic BFT.Int8)
            , ("int_fast16_t",   "Int16",    BindingSpec.Basic BFT.Int16)
            , ("int_fast32_t",   "Int32",    BindingSpec.Basic BFT.Int32)
            , ("int_fast64_t",   "Int64",    BindingSpec.Basic BFT.Int64)
            , ("uint_fast8_t",   "Word8",    BindingSpec.Basic BFT.Word8)
            , ("uint_fast16_t",  "Word16",   BindingSpec.Basic BFT.Word16)
            , ("uint_fast32_t",  "Word32",   BindingSpec.Basic BFT.Word32)
            , ("uint_fast64_t",  "Word64",   BindingSpec.Basic BFT.Word64)
            , ("intmax_t",       "CIntMax",  BindingSpec.Builtin BFT.CIntMax)
            , ("uintmax_t",      "CUIntMax", BindingSpec.Builtin BFT.CUIntMax)
            , ("intptr_t",       "CIntPtr",  BindingSpec.Builtin BFT.CIntPtr)
            , ("uintptr_t",      "CUIntPtr", BindingSpec.Builtin BFT.CUIntPtr)
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
        let hsR = mkHsR "CDivT" ["cDivT_quot", "cDivT_rem"]
        in  mkType "div_t"     "CDivT"     cD hsR divI ["stdlib.h"]
      , let hsR = mkHsR "CLdivT" ["cLdivT_quot", "cLdivT_rem"]
        in  mkType "ldiv_t"    "CLdivT"    cD hsR divI ["stdlib.h"]
      , let hsR = mkHsR "CLldivT" ["cLldivT_quot", "cLldivT_rem"]
        in  mkType "lldiv_t"   "CLldivT"   cD hsR divI ["stdlib.h"]
      , let hsR = mkHsR "CImaxdivT" ["cImaxdivT_quot", "cImaxdivT_rem"]
        in  mkType "imaxdiv_t" "CImaxdivT" cD hsR divI ["inttypes.h"]
      ]

    stdTypes :: [(CTypeKV, HsTypeKV)]
    stdTypes = [
        mkTypeN "size_t" "CSize" cD intI (Just (BindingSpec.Builtin BFT.CSize)) [
            "signal.h"
          , "stddef.h"
          , "stdio.h"
          , "stdlib.h"
          , "string.h"
          , "time.h"
          , "uchar.h"
          , "wchar.h"
          ]
      , mkTypeN "ptrdiff_t" "CPtrdiff" cD intI (Just (BindingSpec.Builtin BFT.CPtrdiff)) ["stddef.h"]
      ]

    nonLocalJumpTypes :: [(CTypeKV, HsTypeKV)]
    nonLocalJumpTypes = [
        mkType "jmp_buf" "CJmpBuf" cO hsO [] ["setjmp.h"]
      ]

    wcharTypes :: [(CTypeKV, HsTypeKV)]
    wcharTypes = [
        mkTypeN "wchar_t" "CWchar" cD intI (Just (BindingSpec.Builtin BFT.CWchar)) [
            "inttypes.h"
          , "stddef.h"
          , "stdlib.h"
          , "wchar.h"
          ]
      , mkTypeN "wint_t"    "CWintT"    cD     intI (Just (BindingSpec.Builtin BFT.CUInt))  ["wchar.h", "wctype.h"]
      , mkType  "mbstate_t" "CMbstateT" cO hsO []                               ["uchar.h", "wchar.h"]
      , mkTypeN "wctrans_t" "CWctransT" cD     eqI  (Just (BindingSpec.Basic BFT.Ptr))      ["wctype.h"]
      , mkTypeN "wctype_t"  "CWctypeT"  cD     eqI  (Just (BindingSpec.Builtin BFT.CULong)) ["wchar.h", "wctype.h"]
      , mkTypeN "char16_t"  "CChar16T"  cD     intI (Just (BindingSpec.Basic BFT.Word16))   ["uchar.h"]
      , mkTypeN "char32_t"  "CChar32T"  cD     intI (Just (BindingSpec.Basic BFT.Word32))   ["uchar.h"]
      ]

    timeTypes :: [(CTypeKV, HsTypeKV)]
    timeTypes = [
        mkTypeN "time_t"  "CTime"  cD timeI (Just (BindingSpec.Builtin BFT.CTime))  ["signal.h", "time.h"]
      , mkTypeN "clock_t" "CClock" cD timeI (Just (BindingSpec.Builtin BFT.CClock)) ["signal.h", "time.h"]
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
        in  mkType "struct tm" "CTm" cD hsR eqI ["time.h"]
      ]

    fileTypes :: [(CTypeKV, HsTypeKV)]
    fileTypes = [
        mkType "FILE"   "CFile" cO hsO [] ["stdio.h", "wchar.h"]
      , mkType "fpos_t" "CFpos" cO hsO [] ["stdio.h"]
      ]

    signalTypes :: [(CTypeKV, HsTypeKV)]
    signalTypes = [
        mkTypeN "sig_atomic_t" "CSigAtomic" cD intI (Just (BindingSpec.Builtin BFT.CSigAtomic)) ["signal.h"]
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
      , Hs.HasBaseForeignType
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
      , Hs.HasBaseForeignType
      ]

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- | Concise alias for the C type 'Map'
type CTypeMap =
  Map C.DeclName [(Set HashIncludeArg, Omittable BindingSpec.CTypeSpec)]

-- | Concise alias for the key and value tuple corresponding to an entry in a
-- 'CTypeMap'
type CTypeKV =
  (C.DeclName, [(Set HashIncludeArg, Omittable BindingSpec.CTypeSpec)])

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
  -> BindingSpec.CTypeRep
  -> BindingSpec.HsTypeRep
  -> [Hs.TypeClass]
  -> [FilePath]
  -> (CTypeKV, HsTypeKV)
mkType t hsIdentifier cTypeRep hsTypeRep insts headers' =
    case C.parseDeclName t of
      Just cDeclName ->
        ( (cDeclName, [(headers, Require cTypeSpec)])
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

-- | Concise aliases for 'BindingSpec.CTypeRepDefault' and
-- 'BindingSpec.CTypeRepOpaque'
cD, cO :: BindingSpec.CTypeRep
cD = BindingSpec.CTypeRepDefault
cO = BindingSpec.CTypeRepOpaque

-- | Concise alias for 'BindingSpec.HsTypeRepOpaque'
hsO :: BindingSpec.HsTypeRep
hsO = BindingSpec.HsTypeRepOpaque

-- | Construct a 'BindingSpec.HsTypeRepRecord' with the specified constructor
-- and field names
mkHsR :: Hs.Identifier -> [Hs.Identifier] -> BindingSpec.HsTypeRep
mkHsR constructorName fieldNames = BindingSpec.HsTypeRepRecord $
    BindingSpec.HsRecordRep {
        hsRecordRepConstructor = Just constructorName
      , hsRecordRepFields      = Just fieldNames
      }

-- | Construct a 'BindingSpec.HsTypeRepNewtype' with the specified constructor
-- name and no field names
--
-- The standard @newtype@ types do not have field names.
mkHsN :: Hs.Identifier -> Maybe BindingSpec.FFIType -> BindingSpec.HsTypeRep
mkHsN constructorName ffitype = BindingSpec.HsTypeRepNewtype $
    BindingSpec.HsNewtypeRep {
        hsNewtypeRepConstructor = Just constructorName
      , hsNewtypeRepField       = Nothing
      , hsNewtypeRepFFIType     = ffitype
      }

-- | Variant of 'mkType' that creates a 'BindingSpec.HsTypeRepNewtype' where the
-- constructor has the same name as the type
mkTypeN ::
     Text
  -> Hs.Identifier
  -> BindingSpec.CTypeRep
  -> [Hs.TypeClass]
  -> Maybe BindingSpec.FFIType
  -> [FilePath]
  -> (CTypeKV, HsTypeKV)
mkTypeN t hsIdentifier cTypeRep insts ffitype headers =
    mkType t hsIdentifier cTypeRep (mkHsN hsIdentifier ffitype) insts headers
