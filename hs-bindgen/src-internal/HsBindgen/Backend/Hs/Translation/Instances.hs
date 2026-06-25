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

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.BindingSpec
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.Language.Haskell
import HsBindgen.Language.Haskell qualified as Hs

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

getDeriveStrat :: Inst.SupportedStrategies -> Maybe (Hs.Strategy ty)
getDeriveStrat supStrats = case supStrats.defStrategy of
    Nothing             -> Nothing
    Just Inst.HsBindgen -> Nothing
    Just Inst.Newtype   -> Just Hs.DeriveNewtype
    Just Inst.Stock     -> Just Hs.DeriveStock

--------------------------------------------------------------------------------

type InstanceMap = Map (Hs.Name NsTypeConstr) (Set Inst.TypeClass)

getInstances ::
     HasCallStack
  => InstanceMap                  -- ^ Current state
  -> Maybe (Hs.Name NsTypeConstr) -- ^ Name of current type (optional)
  -> Set Inst.TypeClass           -- ^ Candidate instances
  -> [Hs.Type]                    -- ^ Dependencies
  -> Set Inst.TypeClass
getInstances instanceMap name = aux
  where
    aux :: Set Inst.TypeClass -> [Hs.Type] -> Set Inst.TypeClass
    aux acc [] = acc
    aux acc (hsType:hsTypes)
      | Set.null acc = acc
      | otherwise = case hsType of
          Hs.PrimType primType ->
            aux (acc /\ getHsPrimTypeInsts primType) hsTypes
          Hs.TypRef name' _
            | Just name' == name -> aux acc hsTypes
            | otherwise -> case Map.lookup name' instanceMap of
                Just instances -> aux (acc /\ instances) hsTypes
                Nothing -> panicPure $ "Type not found: " ++ show name'
          Hs.ConstArray _n hsType' ->
            -- TODO: instance resolution for 'IsArray' is currently
            -- special-cased because for 'IsArray' instances we don't have to
            -- check all dependencies. See issue #1739.
            (if Inst.IsArray `Set.member` acc
              then Set.insert Inst.IsArray
              else id) $
            -- constrain by ConstantArray item type in next step
            aux (acc /\ cArrayInsts) $ hsType' : hsTypes
          Hs.IncompleteArray hsType' ->
            -- TODO: instance resolution for 'IsArray' is currently
            -- special-cased because for 'IsArray' instances we don't have to
            -- check all dependencies. See issue #1739.
            (if Inst.IsArray `Set.member` acc
              then Set.insert Inst.IsArray
              else id) $
            -- constrain by Array item type in next step
            aux (acc /\ arrayInsts) $ hsType' : hsTypes
          Hs.PtrArrayElem{} -> aux (acc /\ ptrInsts) hsTypes
          Hs.PtrConstArrayElem{} -> aux (acc /\ ptrInsts) hsTypes
          Hs.Ptr{} -> aux (acc /\ ptrInsts) hsTypes
          Hs.FunPtr{} -> aux (acc /\ ptrInsts) hsTypes
          Hs.StablePtr{} -> aux (acc /\ ptrInsts) hsTypes
          Hs.PtrConst{} -> aux (acc /\ ptrInsts) hsTypes
          Hs.IO t  -> aux (acc /\ ioInsts) (t : hsTypes)
          Hs.Fun arg res -> aux (acc /\ funInsts) (arg : res : hsTypes)
          Hs.ExtBinding _ref _cTypeSpec hsTypeSpec _ ->
            let acc' = acc /\ hsTypeSpecInsts hsTypeSpec
            in  aux acc' hsTypes
          Hs.ByteArray{} ->
            let acc' = acc /\ Set.fromList [Inst.Eq, Inst.Ord, Inst.Show]
            in  aux acc' hsTypes
          Hs.SizedByteArray{} ->
            let acc' = acc /\ Set.fromList [Inst.Eq, Inst.Show]
            in  aux acc' hsTypes
          Hs.Block{} ->
            aux (blockInsts /\ acc) (hsTypes)
          Hs.ComplexType primType -> aux (acc /\ getHsPrimTypeInsts primType) hsTypes
          Hs.StrLit{} -> Set.empty
          -- TODO <https://github.com/well-typed/hs-bindgen/issues/1572>
          -- Instances for 'WithFlam'.
          Hs.WithFlam{} -> Set.empty
          Hs.EquivStorable{} -> Set.empty

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
      , Inst.HasFFIType
      , Inst.Ord
      , Inst.ReadRaw
      , Inst.Show
      , Inst.StaticSize
      , Inst.Storable
      , Inst.WriteRaw
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

getHsPrimTypeInsts :: Hs.PrimType -> Set Inst.TypeClass
getHsPrimTypeInsts = \case
    Hs.PrimVoid    -> voidInsts
    Hs.PrimUnit    -> unitInsts
    Hs.PrimChar    -> charInsts
    Hs.PrimInt     -> integralInsts
    Hs.PrimDouble  -> floatingInsts
    Hs.PrimFloat   -> floatingInsts
    Hs.PrimBool    -> boolInsts
    Hs.PrimInt8    -> integralInsts
    Hs.PrimInt16   -> integralInsts
    Hs.PrimInt32   -> integralInsts
    Hs.PrimInt64   -> integralInsts
    Hs.PrimWord    -> integralInsts
    Hs.PrimWord8   -> integralInsts
    Hs.PrimWord16  -> integralInsts
    Hs.PrimWord32  -> integralInsts
    Hs.PrimWord64  -> integralInsts
    Hs.PrimCChar   -> integralInsts
    Hs.PrimCSChar  -> integralInsts
    Hs.PrimCUChar  -> integralInsts
    Hs.PrimCShort  -> integralInsts
    Hs.PrimCUShort -> integralInsts
    Hs.PrimCInt    -> integralInsts
    Hs.PrimCUInt   -> integralInsts
    Hs.PrimCLong   -> integralInsts
    Hs.PrimCULong  -> integralInsts
    Hs.PrimCLLong  -> integralInsts
    Hs.PrimCULLong -> integralInsts
    Hs.PrimCBool   -> integralInsts
    Hs.PrimCFloat  -> floatingInsts
    Hs.PrimCDouble -> floatingInsts
  where
    boolInsts :: Set Inst.TypeClass
    boolInsts = Set.fromList [
        Inst.Bits
      , Inst.Bounded
      , Inst.Enum
      , Inst.Eq
      , Inst.FiniteBits
      , Inst.Generic
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
      , Inst.Generic
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
      , Inst.Generic
      , Inst.Ix
      , Inst.Ord
      , Inst.Read
      , Inst.Show
      ]
