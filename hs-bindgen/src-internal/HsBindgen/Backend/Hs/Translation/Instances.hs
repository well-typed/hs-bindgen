-- | Instance resolution
--
-- Instance resolution as currently implemented in the backend is /not/ feature
-- complete.  It does not take into account prescriptive binding specifications.
-- One therefore cannot generate/derive an instance that is not
-- generated/derived by default or use a non-default strategy, and it cannot
-- generate instances with constraints.  Instance resolution will be implemented
-- in a completely different way in the frontend to support these features,
-- where problems may emit warnings or even drop declarations.
module HsBindgen.Backend.Hs.Translation.Instances (
    getCandidateInsts
  , getDeriveStrat
  , InstanceMap
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

--------------------------------------------------------------------------------

-- | Get candidate instances for instance resolution
--
-- This function returns all instances that /may/ be generated/derived for a
-- type.
getCandidateInsts ::
     Map Inst.TypeClass Inst.SupportedStrategies
  -> Set Inst.TypeClass
getCandidateInsts supInsts = Set.fromList $ catMaybes [
      clss <$ supStrats.defStrategy
    | (clss, supStrats) <- Map.assocs supInsts
    , supStrats.dependency == Inst.Dependent
    ]

getDeriveStrat :: Inst.SupportedStrategies -> Maybe (Strategy ty)
getDeriveStrat supStrats = case supStrats.defStrategy of
    Nothing             -> Nothing
    Just Inst.HsBindgen -> Nothing
    Just Inst.Newtype   -> Just DeriveNewtype
    Just Inst.Stock     -> Just DeriveStock

--------------------------------------------------------------------------------

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
          HsPrimType primType -> aux (acc /\ getHsPrimTypeInsts primType) hsTypes
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
          HsComplexType primType -> aux (acc /\ getHsPrimTypeInsts primType) hsTypes
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

getHsPrimTypeInsts :: HsPrimType -> Set Inst.TypeClass
getHsPrimTypeInsts = \case
    HsPrimVoid       -> voidInsts
    HsPrimUnit       -> unitInsts
    HsPrimCStringLen -> cStringLenInsts
    HsPrimCPtrdiff   -> integralInsts
    HsPrimChar       -> charInsts
    HsPrimInt        -> integralInsts
    HsPrimDouble     -> floatingInsts
    HsPrimFloat      -> floatingInsts
    HsPrimBool       -> boolInsts
    HsPrimInt8       -> integralInsts
    HsPrimInt16      -> integralInsts
    HsPrimInt32      -> integralInsts
    HsPrimInt64      -> integralInsts
    HsPrimWord       -> integralInsts
    HsPrimWord8      -> integralInsts
    HsPrimWord16     -> integralInsts
    HsPrimWord32     -> integralInsts
    HsPrimWord64     -> integralInsts
    HsPrimCChar      -> integralInsts
    HsPrimCSChar     -> integralInsts
    HsPrimCUChar     -> integralInsts
    HsPrimCShort     -> integralInsts
    HsPrimCUShort    -> integralInsts
    HsPrimCInt       -> integralInsts
    HsPrimCUInt      -> integralInsts
    HsPrimCLong      -> integralInsts
    HsPrimCULong     -> integralInsts
    HsPrimCLLong     -> integralInsts
    HsPrimCULLong    -> integralInsts
    HsPrimCBool      -> integralInsts
    HsPrimCFloat     -> floatingInsts
    HsPrimCDouble    -> floatingInsts
  where
    boolInsts :: Set Inst.TypeClass
    boolInsts = Set.fromList [
        Inst.Bits
      , Inst.Bounded
      , Inst.Enum
      , Inst.Eq
      , Inst.FiniteBits
      , Inst.HasFFIType
      , Inst.Ix
      , Inst.Ord
      , Inst.Read
      , Inst.ReadRaw
      , Inst.Show
      , Inst.StaticSize
      , Inst.Storable
      , Inst.WriteRaw
      ]

    charInsts :: Set Inst.TypeClass
    charInsts = Set.fromList [
        Inst.Bounded
      , Inst.Enum
      , Inst.Eq
      , Inst.HasFFIType
      , Inst.Ix
      , Inst.Ord
      , Inst.Prim
      , Inst.Read
      , Inst.ReadRaw
      , Inst.Show
      , Inst.StaticSize
      , Inst.Storable
      , Inst.WriteRaw
      ]

    cStringLenInsts :: Set Inst.TypeClass
    cStringLenInsts = Set.fromList [
        Inst.Eq
      , Inst.Ord
      , Inst.Show
      ]

    floatingInsts :: Set Inst.TypeClass
    floatingInsts = Set.fromList [
        Inst.Enum
      , Inst.Eq
      , Inst.Floating
      , Inst.Fractional
      , Inst.HasFFIType
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
      ]

    integralInsts :: Set Inst.TypeClass
    integralInsts = Set.fromList [
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

    unitInsts :: Set Inst.TypeClass
    unitInsts = Set.fromList [
        Inst.Eq
      , Inst.HasFFIType
      , Inst.Ord
      , Inst.Read
      , Inst.ReadRaw
      , Inst.Show
      , Inst.StaticSize
      , Inst.Storable
      , Inst.WriteRaw
      ]

    voidInsts :: Set Inst.TypeClass
    voidInsts = Set.fromList [
        Inst.Eq
      , Inst.Ix
      , Inst.Ord
      , Inst.Read
      , Inst.Show
      ]
