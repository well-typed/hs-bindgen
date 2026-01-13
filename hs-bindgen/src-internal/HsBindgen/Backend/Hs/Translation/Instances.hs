-- | Type classes, instances, and constraints
module HsBindgen.Backend.Hs.Translation.Instances (
    InstanceMap
  , getInstances
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import HsBindgen.Backend.Hs.AST
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.BindingSpec
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
import HsBindgen.Language.Haskell

type InstanceMap = Map (Hs.Name NsTypeConstr) (Set Inst.TypeClass)

getInstances ::
     HasCallStack
  => InstanceMap                   -- ^ Current state
  -> Maybe (Hs.Name NsTypeConstr)  -- ^ Name of current type (optionaL)
  -> Set Inst.TypeClass            -- ^ Candidate instances
  -> [HsType]                      -- ^ Dependencies
  -> Set Inst.TypeClass
getInstances instanceMap name = aux
  where
    aux :: Set Inst.TypeClass -> [HsType] -> Set Inst.TypeClass
    aux acc [] = acc
    aux acc (hsType:hsTypes)
      | Set.null acc = acc
      | otherwise = case hsType of
          HsPrimType primType -> aux (acc /\ hsPrimTypeInsts primType) hsTypes
          HsTypRef name' _
            | Just name' == name -> aux acc hsTypes
            | otherwise -> case Map.lookup name' instanceMap of
                Just instances -> aux (acc /\ instances) hsTypes
                Nothing -> panicPure $ "type not found: " ++ show name'
          HsConstArray _n hsType' ->
            -- constrain by ConstantArray item type in next step
            aux (acc /\ cArrayInsts) $ hsType' : hsTypes
          HsIncompleteArray hsType' ->
            -- constrain by Array item type in next step
            aux (acc /\ arrayInsts) $ hsType' : hsTypes
          HsPtr{} -> aux (acc /\ ptrInsts) hsTypes
          HsFunPtr{} -> aux (acc /\ ptrInsts) hsTypes
          HsStablePtr{} -> aux (acc /\ ptrInsts) hsTypes
          HsPtrConst{} -> aux (acc /\ ptrInsts) hsTypes
          HsIO t  -> aux (acc /\ ioInsts) (t : hsTypes)
          HsFun arg res -> aux (acc /\ funInsts) (arg : res : hsTypes)
          HsExtBinding _ref _cTypeSpec hsTypeSpec _ ->
            let acc' = acc /\ hsTypeSpecInsts hsTypeSpec
            in  aux acc' hsTypes
          HsByteArray{} ->
            let acc' = acc /\ Set.fromList [Inst.Eq, Inst.Ord, Inst.Show]
            in  aux acc' hsTypes
          HsSizedByteArray{} ->
            let acc' = acc /\ Set.fromList [Inst.Eq, Inst.Show]
            in  aux acc' hsTypes
          HsBlock t ->
            aux (blockInsts /\ acc) (t:hsTypes)
          HsComplexType primType -> aux (acc /\ hsPrimTypeInsts primType) hsTypes
          HsStrLit{} -> Set.empty
          -- TODO https://github.com/well-typed/hs-bindgen/issues/1572:
          -- Instances for 'WithFlam'.
          HsWithFlam{} -> Set.empty

    (/\) :: Ord a => Set a -> Set a -> Set a
    (/\) = Set.intersection

    ioInsts :: Set Inst.TypeClass
    ioInsts = Set.singleton Inst.HasFFIType

    funInsts :: Set Inst.TypeClass
    funInsts = Set.singleton Inst.HasFFIType

    blockInsts :: Set Inst.TypeClass
    blockInsts = Set.singleton Inst.HasFFIType

    hsPrimTypeInsts :: HsPrimType -> Set Inst.TypeClass
    hsPrimTypeInsts = \case
      HsPrimVoid -> Set.fromList [
          Inst.Eq
        , Inst.Ix
        , Inst.Ord
        , Inst.Read
        , Inst.Show
        ]
      HsPrimUnit -> unitInsts
      HsPrimCStringLen -> Set.fromList [Inst.Eq, Inst.Ord, Inst.Show]
      HsPrimCPtrdiff -> integralInsts
      HsPrimChar -> Set.fromList [Inst.Eq, Inst.Ord, Inst.Show, Inst.Read]
      HsPrimInt -> integralInsts
      HsPrimDouble -> floatingInsts
      HsPrimFloat -> floatingInsts
      HsPrimBool -> integralInsts
      HsPrimInt8 -> integralInsts
      HsPrimInt16 -> integralInsts
      HsPrimInt32 -> integralInsts
      HsPrimInt64 -> integralInsts
      HsPrimWord -> integralInsts
      HsPrimWord8 -> integralInsts
      HsPrimWord16 -> integralInsts
      HsPrimWord32 -> integralInsts
      HsPrimWord64 -> integralInsts
      HsPrimCChar -> integralInsts
      HsPrimCSChar -> integralInsts
      HsPrimCUChar -> integralInsts
      HsPrimCShort -> integralInsts
      HsPrimCUShort -> integralInsts
      HsPrimCInt -> integralInsts
      HsPrimCUInt -> integralInsts
      HsPrimCLong -> integralInsts
      HsPrimCULong -> integralInsts
      HsPrimCLLong -> integralInsts
      HsPrimCULLong -> integralInsts
      HsPrimCBool -> integralInsts
      HsPrimCFloat -> floatingInsts
      HsPrimCDouble -> floatingInsts

    unitInsts :: Set Inst.TypeClass
    unitInsts = Set.fromList [
        Inst.Eq
      , Inst.Ord
      , Inst.Read
      , Inst.ReadRaw
      , Inst.Show
      , Inst.StaticSize
      , Inst.Storable
      , Inst.WriteRaw
      , Inst.HasFFIType
      ]

    integralInsts :: Set Inst.TypeClass
    integralInsts = Set.fromList [
        Inst.Bitfield
      , Inst.Bits
      , Inst.Bounded
      , Inst.Enum
      , Inst.Eq
      , Inst.FiniteBits
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
      , Inst.HasFFIType
      ]

    floatingInsts :: Set Inst.TypeClass
    floatingInsts = Set.fromList [
        Inst.Enum
      , Inst.Eq
      , Inst.Floating
      , Inst.Fractional
      , Inst.Num
      , Inst.Ord
      , Inst.Prim
      , Inst.Read
      , Inst.ReadRaw
      , Inst.Real
      , Inst.RealFloat
      , Inst.RealFrac
      , Inst.Show
      , Inst.StaticSize
      , Inst.Storable
      , Inst.WriteRaw
      , Inst.HasFFIType
      ]

    ptrInsts :: Set Inst.TypeClass
    ptrInsts = Set.fromList [
        Inst.Eq
      , Inst.Ord
      , Inst.ReadRaw
      , Inst.Show
      , Inst.StaticSize
      , Inst.Storable
      , Inst.WriteRaw
      , Inst.HasFFIType
      ]

    cArrayInsts :: Set Inst.TypeClass
    cArrayInsts = Set.fromList [
        Inst.Eq
      , Inst.ReadRaw
      , Inst.Show
      , Inst.StaticSize
      , Inst.Storable
      , Inst.WriteRaw
      ]

    arrayInsts :: Set Inst.TypeClass
    arrayInsts = Set.fromList [
        Inst.Eq
      , Inst.Show
      ]

    hsTypeSpecInsts :: HsTypeSpec -> Set Inst.TypeClass
    hsTypeSpecInsts hsTypeSpec = Set.fromAscList [
        cls
      | (cls, Require{}) <- Map.toAscList hsTypeSpec.instances
      ]
