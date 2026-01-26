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
import HsBindgen.Language.Haskell

type InstanceMap = Map (Hs.Name NsTypeConstr) (Set TypeClass)

getInstances ::
     HasCallStack
  => InstanceMap               -- ^ Current state
  -> Maybe (Hs.Name NsTypeConstr) -- ^ Name of current type (optionaL)
  -> Set TypeClass             -- ^ Candidate instances
  -> [HsType]                  -- ^ Dependencies
  -> Set TypeClass
getInstances instanceMap name = aux
  where
    aux :: Set TypeClass -> [HsType] -> Set TypeClass
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
          HsConstPtr{} -> aux (acc /\ ptrInsts) hsTypes
          HsIO t  -> aux (acc /\ ioInsts) (t : hsTypes)
          HsFun arg res -> aux (acc /\ funInsts) (arg : res : hsTypes)
          HsExtBinding _ref _cTypeSpec hsTypeSpec _ ->
            let acc' = acc /\ hsTypeSpecInsts hsTypeSpec
            in  aux acc' hsTypes
          HsByteArray{} ->
            let acc' = acc /\ Set.fromList [Eq, Ord, Show]
            in  aux acc' hsTypes
          HsSizedByteArray{} ->
            let acc' = acc /\ Set.fromList [Eq, Show]
            in  aux acc' hsTypes
          HsBlock t ->
            aux (blockInsts /\ acc) (t:hsTypes)
          HsComplexType primType -> aux (acc /\ hsPrimTypeInsts primType) hsTypes
          HsStrLit{} -> Set.empty
          -- TODO https://github.com/well-typed/hs-bindgen/issues/1572:
          -- Instances for 'WithFlexibleArrayMember'.
          HsWithFlexibleArrayMember{} -> Set.empty

    (/\) :: Ord a => Set a -> Set a -> Set a
    (/\) = Set.intersection

    ioInsts :: Set TypeClass
    ioInsts = Set.singleton HasBaseForeignType

    funInsts :: Set TypeClass
    funInsts = Set.singleton HasBaseForeignType

    blockInsts :: Set TypeClass
    blockInsts = Set.singleton HasBaseForeignType

    hsPrimTypeInsts :: HsPrimType -> Set TypeClass
    hsPrimTypeInsts = \case
      HsPrimVoid       -> Set.fromList [Eq, Ix, Ord, Read, Show]
      HsPrimUnit       -> unitInsts
      HsPrimCStringLen -> Set.fromList [Eq, Ord, Show]
      HsPrimCPtrdiff -> integralInsts
      HsPrimChar -> Set.fromList [Eq, Ord, Show, Read]
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

    unitInsts :: Set TypeClass
    unitInsts = Set.fromList [
        Eq
      , Ord
      , Read
      , ReadRaw
      , Show
      , StaticSize
      , Storable
      , WriteRaw
      , HasBaseForeignType
      ]

    integralInsts :: Set TypeClass
    integralInsts = Set.fromList [
        Bitfield
      , Bits
      , Bounded
      , Enum
      , Eq
      , FiniteBits
      , Integral
      , Ix
      , Num
      , Ord
      , Prim
      , Read
      , ReadRaw
      , Real
      , Show
      , StaticSize
      , Storable
      , WriteRaw
      , HasBaseForeignType
      ]

    floatingInsts :: Set TypeClass
    floatingInsts = Set.fromList [
        Enum
      , Eq
      , Floating
      , Fractional
      , Num
      , Ord
      , Prim
      , Read
      , ReadRaw
      , Real
      , RealFloat
      , RealFrac
      , Show
      , StaticSize
      , Storable
      , WriteRaw
      , HasBaseForeignType
      ]

    ptrInsts :: Set TypeClass
    ptrInsts = Set.fromList [
        Eq
      , Ord
      , ReadRaw
      , Show
      , StaticSize
      , Storable
      , WriteRaw
      , HasBaseForeignType
      ]

    cArrayInsts :: Set TypeClass
    cArrayInsts = Set.fromList [
        Eq
      , ReadRaw
      , Show
      , StaticSize
      , Storable
      , WriteRaw
      ]

    arrayInsts :: Set TypeClass
    arrayInsts = Set.fromList [
        Eq
      , Show
      ]

    hsTypeSpecInsts :: HsTypeSpec -> Set TypeClass
    hsTypeSpecInsts hsTypeSpec = Set.fromAscList [
        cls
      | (cls, Require{}) <- Map.toAscList hsTypeSpec.instances
      ]
