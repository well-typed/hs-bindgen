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
      let aux (t, hsIdentifier, bft) =
            mkTypeN t hsIdentifier cD intI (Just bft) ["inttypes.h", "stdint.h"]
      in  map aux [
              ("int8_t",         "Int8",     Hs.Int8)
            , ("int16_t",        "Int16",    Hs.Int16)
            , ("int32_t",        "Int32",    Hs.Int32)
            , ("int64_t",        "Int64",    Hs.Int64)
            , ("uint8_t",        "Word8",    Hs.Word8)
            , ("uint16_t",       "Word16",   Hs.Word16)
            , ("uint32_t",       "Word32",   Hs.Word32)
            , ("uint64_t",       "Word64",   Hs.Word64)
            , ("int_least8_t",   "Int8",     Hs.Int8)
            , ("int_least16_t",  "Int16",    Hs.Int16)
            , ("int_least32_t",  "Int32",    Hs.Int32)
            , ("int_least64_t",  "Int64",    Hs.Int64)
            , ("uint_least8_t",  "Word8",    Hs.Word8)
            , ("uint_least16_t", "Word16",   Hs.Word16)
            , ("uint_least32_t", "Word32",   Hs.Word32)
            , ("uint_least64_t", "Word64",   Hs.Word64)
            , ("int_fast8_t",    "Int8",     Hs.Int8)
            , ("int_fast16_t",   "Int16",    Hs.Int16)
            , ("int_fast32_t",   "Int32",    Hs.Int32)
            , ("int_fast64_t",   "Int64",    Hs.Int64)
            , ("uint_fast8_t",   "Word8",    Hs.Word8)
            , ("uint_fast16_t",  "Word16",   Hs.Word16)
            , ("uint_fast32_t",  "Word32",   Hs.Word32)
            , ("uint_fast64_t",  "Word64",   Hs.Word64)
            , ("intmax_t",       "CIntMax",  Hs.CIntMax)
            , ("uintmax_t",      "CUIntMax", Hs.CUIntMax)
            , ("intptr_t",       "CIntPtr",  Hs.CIntPtr)
            , ("uintptr_t",      "CUIntPtr", Hs.CUIntPtr)
            ]

    floatingTypes :: [(CTypeKV, HsTypeKV)]
    floatingTypes =
      let aux (t, hsIdentifier, bft) = mkType t hsIdentifier cO hsO [] bft ["fenv.h"]
      in  map aux [
              ("fenv_t",    "CFenvT",    Nothing)
            , ("fexcept_t", "CFexceptT", Nothing)
            ]

    mathTypes :: [(CTypeKV, HsTypeKV)]
    mathTypes = [
        let hsR = mkHsR "CDivT" ["cDivT_quot", "cDivT_rem"]
        in  mkType "div_t"     "CDivT"     cD hsR divI Nothing ["stdlib.h"]
      , let hsR = mkHsR "CLdivT" ["cLdivT_quot", "cLdivT_rem"]
        in  mkType "ldiv_t"    "CLdivT"    cD hsR divI Nothing ["stdlib.h"]
      , let hsR = mkHsR "CLldivT" ["cLldivT_quot", "cLldivT_rem"]
        in  mkType "lldiv_t"   "CLldivT"   cD hsR divI Nothing ["stdlib.h"]
      , let hsR = mkHsR "CImaxdivT" ["cImaxdivT_quot", "cImaxdivT_rem"]
        in  mkType "imaxdiv_t" "CImaxdivT" cD hsR divI Nothing ["inttypes.h"]
      ]

    stdTypes :: [(CTypeKV, HsTypeKV)]
    stdTypes = [
        mkTypeN "size_t" "CSize" cD intI (Just Hs.CSize) [
            "signal.h"
          , "stddef.h"
          , "stdio.h"
          , "stdlib.h"
          , "string.h"
          , "time.h"
          , "uchar.h"
          , "wchar.h"
          ]
      , mkTypeN "ptrdiff_t" "CPtrdiff" cD intI (Just Hs.CPtrdiff) ["stddef.h"]
      ]

    nonLocalJumpTypes :: [(CTypeKV, HsTypeKV)]
    nonLocalJumpTypes = [
        mkType "jmp_buf" "CJmpBuf" cO hsO [] Nothing ["setjmp.h"]
      ]

    wcharTypes :: [(CTypeKV, HsTypeKV)]
    wcharTypes = [
        mkTypeN "wchar_t" "CWchar" cD intI (Just Hs.CWchar) [
            "inttypes.h"
          , "stddef.h"
          , "stdlib.h"
          , "wchar.h"
          ]
      , mkTypeN "wint_t"    "CWintT"    cD     intI (Just Hs.CUInt)  ["wchar.h", "wctype.h"]
      , mkType  "mbstate_t" "CMbstateT" cO hsO []   Nothing          ["uchar.h", "wchar.h"]
      , mkTypeN "wctrans_t" "CWctransT" cD     eqI  (Just Hs.Ptr)    ["wctype.h"]
      , mkTypeN "wctype_t"  "CWctypeT"  cD     eqI  (Just Hs.CULong) ["wchar.h", "wctype.h"]
      , mkTypeN "char16_t"  "CChar16T"  cD     intI (Just Hs.Word16) ["uchar.h"]
      , mkTypeN "char32_t"  "CChar32T"  cD     intI (Just Hs.Word32) ["uchar.h"]
      ]

    timeTypes :: [(CTypeKV, HsTypeKV)]
    timeTypes = [
        mkTypeN "time_t"  "CTime"  cD timeI (Just Hs.CTime) ["signal.h", "time.h"]
      , mkTypeN "clock_t" "CClock" cD timeI (Just Hs.CClock) ["signal.h", "time.h"]
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
        in  mkType "struct tm" "CTm" cD hsR eqI Nothing ["time.h"]
      ]

    fileTypes :: [(CTypeKV, HsTypeKV)]
    fileTypes = [
        mkType "FILE"   "CFile" cO hsO [] Nothing ["stdio.h", "wchar.h"]
      , mkType "fpos_t" "CFpos" cO hsO [] Nothing ["stdio.h"]
      ]

    signalTypes :: [(CTypeKV, HsTypeKV)]
    signalTypes = [
        mkTypeN "sig_atomic_t" "CSigAtomic" cD intI (Just Hs.CSigAtomic) ["signal.h"]
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
  -> Maybe Hs.BasicForeignType
  -> [FilePath]
  -> (CTypeKV, HsTypeKV)
mkType t hsIdentifier cTypeRep hsTypeRep insts bft headers' =
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
      , hsTypeSpecBaseForeignType = bft
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
mkHsN :: Hs.Identifier -> BindingSpec.HsTypeRep
mkHsN constructorName = BindingSpec.HsTypeRepNewtype $
    BindingSpec.HsNewtypeRep {
        hsNewtypeRepConstructor = Just constructorName
      , hsNewtypeRepField       = Nothing
      }

-- | Variant of 'mkType' that creates a 'BindingSpec.HsTypeRepNewtype' where the
-- constructor has the same name as the type
mkTypeN ::
     Text
  -> Hs.Identifier
  -> BindingSpec.CTypeRep
  -> [Hs.TypeClass]
  -> Maybe Hs.BasicForeignType
  -> [FilePath]
  -> (CTypeKV, HsTypeKV)
mkTypeN t hsIdentifier cTypeRep insts bft headers =
    mkType t hsIdentifier cTypeRep (mkHsN hsIdentifier) insts bft headers
