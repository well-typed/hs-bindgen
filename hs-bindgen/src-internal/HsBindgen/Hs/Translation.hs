{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Low-level translation of the C header to a Haskell module
--
-- TODO: This module is intended to implement the following milestones:
--
-- * Milestone 1: @Storable@ instances
--   <https://github.com/well-typed/hs-bindgen/milestone/2>
-- * Milestone 2: Low-level API
--   <https://github.com/well-typed/hs-bindgen/milestone/3>
module HsBindgen.Hs.Translation (
    TranslationOpts(..)
  , defaultTranslationOpts
  , generateDeclarations
    -- * leaky exports:
    --   perfectly, translation will happen in *this* module.
  , integralType
  , floatingType
  ) where

import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Type.Nat (SNatI, induction)
import Data.Vec.Lazy qualified as Vec
import GHC.Exts qualified as IsList (IsList (..))

import C.Char qualified
import C.Type qualified ( FloatingType(..), IntegralType(IntLike) )
import Clang.Paths
import HsBindgen.C.AST qualified as C
import HsBindgen.C.AST.Type qualified as C
import HsBindgen.C.Tc.Macro qualified as Macro
import HsBindgen.Errors
import HsBindgen.ExtBindings (HsTypeClass)
import HsBindgen.ExtBindings qualified as ExtBindings
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type
import HsBindgen.SHs.AST qualified as SHs
import HsBindgen.SHs.Translation qualified as SHs
import HsBindgen.Hs.NameMangler
import HsBindgen.Imports
import HsBindgen.ModuleUnique
import HsBindgen.NameHint
import HsBindgen.PrettyC qualified as PC

import DeBruijn (Add (..), EmptyCtx, Idx (..), pattern I1, pattern I2, weaken, Env (..), zipWithEnv, tabulateEnv, sizeEnv)

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Translation options
--
-- These options allow users to specify instances that /may/ be derived for all
-- structs, enums, and typedefs, along with the strategy to use.  Instances are
-- only derived when possible, however.  For example, an @Eq@ instance may only
-- be derived if all fields have @Eq@ instances.  Note that type classes that
-- @hs-bindgen@ generates instances for must not be included in this
-- configuration.
data TranslationOpts = TranslationOpts {
      -- | Default set of classes to derive for structs
      translationDeriveStruct :: [(Hs.Strategy Hs.HsType, HsTypeClass)]

      -- | Default set of classes to derive for enums
    , translationDeriveEnum :: [(Hs.Strategy Hs.HsType, HsTypeClass)]

      -- | Default set of classes to derive for typedefs
    , translationDeriveTypedef :: [(Hs.Strategy Hs.HsType, HsTypeClass)]
    }
  deriving stock (Show)

defaultTranslationOpts :: TranslationOpts
defaultTranslationOpts = TranslationOpts {
      translationDeriveStruct = [
          (Hs.DeriveStock, Hs.Show)
        , (Hs.DeriveStock, Hs.Eq)
        ]
    , translationDeriveEnum = [
          (Hs.DeriveStock, Hs.Eq)
        , (Hs.DeriveStock, Hs.Ord)
        ]
    , translationDeriveTypedef = [
          (Hs.DeriveStock, Hs.Eq)
        , (Hs.DeriveStock, Hs.Ord)
        , (Hs.DeriveStock, Hs.Read)
        , (Hs.DeriveStock, Hs.Show)
        , (Hs.DeriveNewtype, Hs.Enum)
        , (Hs.DeriveNewtype, Hs.Ix)
        , (Hs.DeriveNewtype, Hs.Bounded)
        , (Hs.DeriveNewtype, Hs.Bits)
        , (Hs.DeriveNewtype, Hs.FiniteBits)
        , (Hs.DeriveNewtype, Hs.Floating)
        , (Hs.DeriveNewtype, Hs.Fractional)
        , (Hs.DeriveNewtype, Hs.Integral)
        , (Hs.DeriveNewtype, Hs.Num)
        , (Hs.DeriveNewtype, Hs.Real)
        , (Hs.DeriveNewtype, Hs.RealFloat)
        , (Hs.DeriveNewtype, Hs.RealFrac)
        ]
    }

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- filepath argument https://github.com/well-typed/hs-bindgen/issues/333
generateDeclarations ::
     TranslationOpts
  -> ModuleUnique
  -> NameMangler
  -> C.Header
  -> [Hs.Decl]
generateDeclarations opts mu nm (C.Header decs) =
    flip State.evalState Map.empty $
      concat <$> mapM (generateDecs opts nm mu typedefs) decs
  where
    typedefs :: Map C.CName C.Type
    typedefs = Map.union actualTypedefs pseudoTypedefs

    -- typedef lookup table
    -- shallow: only one layer of typedefs is stripped.
    actualTypedefs :: Map C.CName C.Type
    actualTypedefs = Map.fromList
        [ (n, t)
        | C.DeclTypedef (C.Typedef { typedefName = n, typedefType = t }) <- decs
        ]

    -- macros also act as "typedef"s
    pseudoTypedefs :: Map C.CName C.Type
    pseudoTypedefs = Map.fromList
        [ (n, ty)
        | C.DeclMacro (C.MacroDecl
          { macroDeclMacro =
              C.Macro
                { C.macroName = n
                , C.macroBody = C.TypeMacro tyNm
                }
          , macroDeclMacroTy =
              Macro.Quant bf
          }) <- decs
        , Macro.isPrimTy bf
        , Right ty <- [C.typeNameType tyNm]
        ]

{-------------------------------------------------------------------------------
  Instance Map
-------------------------------------------------------------------------------}

type InstanceMap = Map (HsName NsTypeConstr) (Set HsTypeClass)

getInstances ::
     InstanceMap         -- ^ Current state
  -> HsName NsTypeConstr -- ^ Name of current type
  -> Set HsTypeClass     -- ^ Candidate instances
  -> [HsType]            -- ^ Dependencies
  -> Set HsTypeClass
getInstances instanceMap name = aux
  where
    aux :: Set HsTypeClass -> [HsType] -> Set HsTypeClass
    aux acc [] = acc
    aux acc (hsType:hsTypes)
      | Set.null acc = acc
      | otherwise = case hsType of
          HsPrimType primType -> aux (acc /\ hsPrimTypeInsts primType) hsTypes
          HsTypRef name'
            | name' == name -> aux acc hsTypes
            | otherwise -> case Map.lookup name' instanceMap of
                Just instances -> aux (acc /\ instances) hsTypes
                Nothing -> panicPure $ "type not found: " ++ show name'
          HsConstArray _n hsType' ->
            -- constrain by ConstantArray item type in next step
            aux (acc /\ cArrayInsts) $ hsType' : hsTypes
          HsPtr{} -> aux (acc /\ ptrInsts) hsTypes
          HsFunPtr{} -> aux (acc /\ ptrInsts) hsTypes
          HsIO{} -> Set.empty
          HsFun{} -> Set.empty
          HsExtBinding extId _cType ->
            let acc' = acc /\ ExtBindings.extIdentifierInstances extId
            in  aux acc' hsTypes
          HsByteArray{} ->
            let acc' = acc /\ Set.fromList [Hs.Eq, Hs.Ord, Hs.Show]
            in  aux acc' hsTypes
          HsSizedByteArray{} ->
            let acc' = acc /\ Set.fromList [Hs.Eq, Hs.Show]
            in  aux acc' hsTypes

    (/\) :: Ord a => Set a -> Set a -> Set a
    (/\) = Set.intersection

    hsPrimTypeInsts :: HsPrimType -> Set HsTypeClass
    hsPrimTypeInsts = \case
      HsPrimVoid       -> Set.fromList [Hs.Eq, Hs.Ix, Hs.Ord, Hs.Read, Hs.Show]
      HsPrimUnit       -> unitInsts
      HsPrimCChar      -> integralInsts
      HsPrimCSChar     -> integralInsts
      HsPrimCUChar     -> integralInsts
      HsPrimCInt       -> integralInsts
      HsPrimCUInt      -> integralInsts
      HsPrimCShort     -> integralInsts
      HsPrimCUShort    -> integralInsts
      HsPrimCLong      -> integralInsts
      HsPrimCULong     -> integralInsts
      HsPrimCPtrDiff   -> integralInsts
      HsPrimCSize      -> integralInsts
      HsPrimCLLong     -> integralInsts
      HsPrimCULLong    -> integralInsts
      HsPrimCBool      -> integralInsts
      HsPrimCFloat     -> floatingInsts
      HsPrimCDouble    -> floatingInsts
      HsPrimCStringLen -> Set.fromList [Hs.Eq, Hs.Ord, Hs.Show]
      HsPrimInt        -> integralInsts

    unitInsts :: Set HsTypeClass
    unitInsts = Set.fromList [
        Hs.Eq
      , Hs.Ord
      , Hs.Read
      , Hs.ReadRaw
      , Hs.Show
      , Hs.StaticSize
      , Hs.Storable
      , Hs.WriteRaw
      ]

    integralInsts :: Set HsTypeClass
    integralInsts = Set.fromList [
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

    floatingInsts :: Set HsTypeClass
    floatingInsts = Set.fromList [
        Hs.Enum
      , Hs.Eq
      , Hs.Floating
      , Hs.Fractional
      , Hs.Num
      , Hs.Ord
      , Hs.Read
      , Hs.ReadRaw
      , Hs.Real
      , Hs.RealFloat
      , Hs.RealFrac
      , Hs.Show
      , Hs.StaticSize
      , Hs.Storable
      , Hs.WriteRaw
      ]

    ptrInsts :: Set HsTypeClass
    ptrInsts = Set.fromList [
        Hs.Eq
      , Hs.Ord
      , Hs.ReadRaw
      , Hs.Show
      , Hs.StaticSize
      , Hs.Storable
      , Hs.WriteRaw
      ]

    cArrayInsts :: Set HsTypeClass
    cArrayInsts = Set.fromList [
        Hs.Eq
      , Hs.ReadRaw
      , Hs.Show
      , Hs.StaticSize
      , Hs.Storable
      , Hs.WriteRaw
      ]

{-------------------------------------------------------------------------------
  Declarations
------------------------------------------------------------------------------}

generateDecs ::
     State.MonadState InstanceMap m
  => TranslationOpts
  -> NameMangler
  -> ModuleUnique
  -> Map C.CName C.Type
  -> C.Decl
  -> m [Hs.Decl]
generateDecs opts nm mu typedefs = \case
    C.DeclStruct struct  -> reifyStructFields struct $ structDecs opts nm struct
    C.DeclUnion union    -> unionDecs nm union
    C.DeclOpaqueStruct o -> opaqueStructDecs nm o
    C.DeclEnum e         -> enumDecs opts nm e
    C.DeclOpaqueEnum o   -> opaqueEnumDecs nm o -- TODO?
    C.DeclTypedef d      -> typedefDecs opts nm d
    C.DeclMacro m        -> macroDecs opts nm m
    C.DeclFunction f     -> return $ functionDecs nm mu typedefs f

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

reifyStructFields ::
     C.Struct
  -> (forall n. SNatI n => Vec n C.StructField -> a)
  -> a
reifyStructFields struct k = Vec.reifyList (C.structFields struct) k

-- | Generate declarations for given C struct
structDecs :: forall n m.
     (SNatI n, State.MonadState InstanceMap m)
  => TranslationOpts
  -> NameMangler
  -> C.Struct
  -> Vec n C.StructField
  -> m [Hs.Decl]
structDecs opts nm struct fields = do
    (insts, decls) <- aux <$> State.get
    State.modify' $ Map.insert structName insts
    return decls
  where
    declPath :: C.DeclPath
    declPath = C.structDeclPath struct

    structName :: HsName NsTypeConstr
    structName = mangle nm $ NameTycon declPath

    structFields :: Vec n Hs.Field
    structFields = flip Vec.map fields $ \f -> Hs.Field {
        fieldName   = mangle nm $ NameField declPath (C.fieldName f)
      , fieldType   = typ nm (C.fieldType f)
      , fieldOrigin = Hs.FieldOriginStructField f
      }

    candidateInsts :: Set HsTypeClass
    candidateInsts = Set.union (Set.singleton Hs.Storable) $
      Set.fromList (snd <$> translationDeriveStruct opts)

    -- everything in aux is state-dependent
    aux :: InstanceMap -> (Set HsTypeClass, [Hs.Decl])
    aux instanceMap = (insts,) $
        structDecl : storableDecl ++ optDecls ++ hasFlamDecl
      where
        insts :: Set HsTypeClass
        insts = getInstances instanceMap structName candidateInsts $
          Hs.fieldType <$> Vec.toList structFields

        hsStruct :: Hs.Struct n
        hsStruct = Hs.Struct {
            structName      = structName
          , structConstr    = mangle nm $ NameDatacon declPath
          , structFields    = structFields
          , structOrigin    = Hs.StructOriginStruct struct
          , structInstances = insts
          }

        structDecl :: Hs.Decl
        structDecl = Hs.DeclData hsStruct

        storableDecl :: [Hs.Decl]
        storableDecl
          | Hs.Storable `Set.notMember` insts = []
          | otherwise = singleton $ Hs.DeclDefineInstance $
              Hs.InstanceStorable hsStruct Hs.StorableInstance {
                  Hs.storableSizeOf    = C.structSizeof struct
                , Hs.storableAlignment = C.structAlignment struct
                , Hs.storablePeek      = Hs.Lambda "ptr" $
                    Hs.Ap (Hs.StructCon hsStruct) $
                      map (peekStructField IZ) (C.structFields struct)
                , Hs.storablePoke      = Hs.Lambda "ptr" $ Hs.Lambda "s" $
                    Hs.makeElimStruct IZ hsStruct $ \wk xs -> Hs.Seq $ toList $
                      Vec.zipWith (pokeStructField (weaken wk I1)) fields xs
                }

        optDecls :: [Hs.Decl]
        optDecls = [
            Hs.DeclDeriveInstance strat clss structName
          | (strat, clss) <- translationDeriveStruct opts
          , clss `Set.member` insts
          ]

        hasFlamDecl :: [Hs.Decl]
        hasFlamDecl = case C.structFlam struct of
          Nothing   -> []
          Just flam -> singleton $ Hs.DeclDefineInstance $ Hs.InstanceHasFLAM
            hsStruct
            (typ nm (C.fieldType flam))
            (C.fieldOffset flam `div` 8)

peekStructField :: Idx ctx -> C.StructField -> Hs.PeekByteOff ctx
peekStructField ptr f = case C.fieldWidth f of
    Nothing -> Hs.PeekByteOff ptr (C.fieldOffset f `div` 8)
    Just w  -> Hs.PeekBitOffWidth ptr (C.fieldOffset f) w

pokeStructField :: Idx ctx -> C.StructField -> Idx ctx -> Hs.PokeByteOff ctx
pokeStructField ptr f i = case C.fieldWidth f of
    Nothing -> Hs.PokeByteOff ptr (C.fieldOffset f `div` 8) i
    Just w  -> Hs.PokeBitOffWidth ptr (C.fieldOffset f) w i

{-------------------------------------------------------------------------------
  Opaque struct and opaque enum
-------------------------------------------------------------------------------}

opaqueStructDecs ::
     State.MonadState InstanceMap m
  => NameMangler
  -> C.OpaqueStruct
  -> m [Hs.Decl]
opaqueStructDecs nm o = do
    State.modify' $ Map.insert name Set.empty
    return [decl]
  where
    name :: HsName NsTypeConstr
    name = mangle nm $ NameTycon $ C.DeclPathName (C.opaqueStructTag o)

    decl :: Hs.Decl
    decl = Hs.DeclEmpty Hs.EmptyData {
        emptyDataName   = name
      , emptyDataOrigin = Hs.EmptyDataOriginOpaqueStruct o
      }

opaqueEnumDecs ::
     State.MonadState InstanceMap m
  => NameMangler
  -> C.OpaqueEnum
  -> m [Hs.Decl]
opaqueEnumDecs nm o = do
    State.modify' $ Map.insert name Set.empty
    return [decl]
  where
    name :: HsName NsTypeConstr
    name = mangle nm $ NameTycon $ C.DeclPathName (C.opaqueEnumTag o)

    decl :: Hs.Decl
    decl = Hs.DeclEmpty Hs.EmptyData {
        emptyDataName   = name
      , emptyDataOrigin = Hs.EmptyDataOriginOpaqueEnum o
      }

{-------------------------------------------------------------------------------
  Unions
-------------------------------------------------------------------------------}

unionDecs ::
     State.MonadState InstanceMap m
  => NameMangler
  -> C.Union
  -> m [Hs.Decl]
unionDecs nm union = do
    decls <- aux <$> State.get
    State.modify' $ Map.insert newtypeName insts
    return decls
  where
    declPath :: C.DeclPath
    declPath = C.unionDeclPath union

    newtypeName :: HsName NsTypeConstr
    newtypeName = mangle nm $ NameTycon declPath

    insts :: Set HsTypeClass
    insts = Set.singleton Hs.Storable

    hsNewtype :: Hs.Newtype
    hsNewtype = Hs.Newtype {
        newtypeName      = newtypeName
      , newtypeConstr    = mangle nm $ NameDatacon declPath
      , newtypeField     = Hs.Field {
            fieldName   = mangle nm $ NameDecon declPath
          , fieldType   = Hs.HsByteArray
          , fieldOrigin = Hs.FieldOriginNone
          }
      , newtypeOrigin    = Hs.NewtypeOriginUnion union
      , newtypeInstances = insts
      }

    newtypeDecl :: Hs.Decl
    newtypeDecl = Hs.DeclNewtype hsNewtype

    storableDecl :: Hs.Decl
    storableDecl =
      Hs.DeclDeriveInstance (Hs.DeriveVia sba) Hs.Storable newtypeName

    sba :: Hs.HsType
    sba =
      HsSizedByteArray
        (fromIntegral (C.unionSizeof union))
        (fromIntegral (C.unionAlignment union))

    -- everything in aux is state-dependent
    aux :: InstanceMap -> [Hs.Decl]
    aux instanceMap = newtypeDecl : storableDecl : accessorDecls
      where
        accessorDecls :: [Hs.Decl]
        accessorDecls = concatMap getAccessorDecls (C.unionFields union)

        getAccessorDecls :: C.UnionField -> [Hs.Decl]
        getAccessorDecls C.UnionField{..} =
          let hsType = typ nm ufieldType
              fInsts = getInstances instanceMap newtypeName insts [hsType]
          in  if Hs.Storable `Set.notMember` fInsts
                then []
                else
                  [ Hs.DeclUnionGetter newtypeName hsType $
                      mangle nm (NameGetter declPath ufieldName)
                  , Hs.DeclUnionSetter newtypeName hsType $
                      mangle nm (NameBuilder declPath ufieldName)
                  ]

{-------------------------------------------------------------------------------
  Enum
-------------------------------------------------------------------------------}

enumDecs ::
     State.MonadState InstanceMap m
  => TranslationOpts
  -> NameMangler
  -> C.Enu
  -> m [Hs.Decl]
enumDecs opts nm e = do
    State.modify' $ Map.insert newtypeName insts
    return $
      newtypeDecl : storableDecl : optDecls ++ cEnumInstanceDecls ++ valueDecls
  where
    declPath :: C.DeclPath
    declPath = C.enumDeclPath e

    newtypeName :: HsName NsTypeConstr
    newtypeName = mangle nm $ NameTycon declPath

    newtypeConstr :: HsName NsConstr
    newtypeConstr = mangle nm $ NameDatacon declPath

    newtypeField :: Hs.Field
    newtypeField = Hs.Field {
        fieldName   = mangle nm $ NameDecon declPath
      , fieldType   = typ nm (C.enumType e)
      , fieldOrigin = Hs.FieldOriginNone
      }

    insts :: Set HsTypeClass
    insts = Set.union (Set.fromList [Hs.Show, Hs.Read, Hs.Storable]) $
      Set.fromList (snd <$> translationDeriveEnum opts)

    hsNewtype :: Hs.Newtype
    hsNewtype = Hs.Newtype {
        newtypeName      = newtypeName
      , newtypeConstr    = newtypeConstr
      , newtypeField     = newtypeField
      , newtypeOrigin    = Hs.NewtypeOriginEnum e
      , newtypeInstances = insts
      }

    newtypeDecl :: Hs.Decl
    newtypeDecl = Hs.DeclNewtype hsNewtype

    hsStruct :: Hs.Struct (S Z)
    hsStruct = Hs.Struct {
        structName      = newtypeName
      , structConstr    = newtypeConstr
      , structFields    = Vec.singleton newtypeField
      , structOrigin    = Hs.StructOriginEnum e
      , structInstances = insts
      }

    storableDecl :: Hs.Decl
    storableDecl = Hs.DeclDefineInstance $
      Hs.InstanceStorable hsStruct Hs.StorableInstance {
          Hs.storableSizeOf    = C.enumSizeof e
        , Hs.storableAlignment = C.enumAlignment e
        , Hs.storablePeek      = Hs.Lambda "ptr" $
            Hs.Ap (Hs.StructCon hsStruct) [ Hs.PeekByteOff IZ 0 ]
        , Hs.storablePoke      = Hs.Lambda "ptr" $ Hs.Lambda "s" $
            Hs.ElimStruct IZ hsStruct (AS AZ) $
              Hs.Seq [ Hs.PokeByteOff I2 0 IZ ]
        }

    optDecls :: [Hs.Decl]
    optDecls = [
        Hs.DeclDeriveInstance strat clss newtypeName
      | (strat, clss) <- translationDeriveEnum opts
      ]

    valueDecls :: [Hs.Decl]
    valueDecls =
        [ Hs.DeclPatSyn Hs.PatSyn
          { patSynName   = mangle nm $ NameDatacon (C.DeclPathName valueName)
          , patSynType   = newtypeName
          , patSynConstr = newtypeConstr
          , patSynValue  = valueValue
          , patSynOrigin = Hs.PatSynOriginEnumValue enumValue
          }
        | enumValue@C.EnumValue{..} <- C.enumValues e
        ]

    cEnumInstanceDecls :: [Hs.Decl]
    cEnumInstanceDecls =
      let vNames = Map.fromListWith (flip (<>)) [ -- preserve source order
              ( Hs.patSynValue pat
              , NonEmpty.singleton (Hs.patSynName pat)
              )
            | Hs.DeclPatSyn pat <- valueDecls
            ]
          mSeqBounds = do
            (minV, minNames) <- Map.lookupMin vNames
            (maxV, maxNames) <- Map.lookupMax vNames
            guard $ maxV - minV + 1 == fromIntegral (Map.size vNames)
            return (NonEmpty.head minNames, NonEmpty.head maxNames)
          fTyp = Hs.fieldType newtypeField
          vStrs = fmap (T.unpack . getHsName) <$> vNames
          cEnumDecl = Hs.DeclDefineInstance $
            Hs.InstanceCEnum hsStruct fTyp vStrs (isJust mSeqBounds)
          cEnumShowDecl = Hs.DeclDefineInstance (Hs.InstanceCEnumShow hsStruct)
          cEnumReadDecl = Hs.DeclDefineInstance (Hs.InstanceCEnumRead hsStruct)
          sequentialCEnumDecl = case mSeqBounds of
            Just (nameMin, nameMax) -> List.singleton . Hs.DeclDefineInstance $
              Hs.InstanceSequentialCEnum hsStruct nameMin nameMax
            Nothing -> []
      in  cEnumDecl : sequentialCEnumDecl ++ [cEnumShowDecl, cEnumReadDecl]

{-------------------------------------------------------------------------------
  Typedef
-------------------------------------------------------------------------------}

typedefDecs ::
     State.MonadState InstanceMap m
  => TranslationOpts
  -> NameMangler
  -> C.Typedef
  -> m [Hs.Decl]
typedefDecs opts nm typedef = do
    (insts, decls) <- aux <$> State.get
    State.modify' $ Map.insert newtypeName insts
    return decls
  where
    declPath :: C.DeclPath
    declPath = C.DeclPathName $ C.typedefName typedef

    newtypeName :: HsName NsTypeConstr
    newtypeName = mangle nm $ NameTycon declPath

    newtypeField :: Hs.Field
    newtypeField = Hs.Field {
        fieldName   = mangle nm $ NameDecon declPath
      , fieldType   = typ nm (C.typedefType typedef)
      , fieldOrigin = Hs.FieldOriginNone
      }

    candidateInsts :: Set HsTypeClass
    candidateInsts = Set.union (Set.singleton Hs.Storable) $
      Set.fromList (snd <$> translationDeriveTypedef opts)

    -- everything in aux is state-dependent
    aux :: InstanceMap -> (Set HsTypeClass, [Hs.Decl])
    aux instanceMap = (insts,) $
        newtypeDecl : storableDecl ++ optDecls
      where
        insts :: Set HsTypeClass
        insts =
          getInstances
            instanceMap
            newtypeName
            candidateInsts
            [Hs.fieldType newtypeField]

        hsNewtype :: Hs.Newtype
        hsNewtype = Hs.Newtype {
            newtypeName      = newtypeName
          , newtypeConstr    = mangle nm $ NameDatacon declPath
          , newtypeField     = newtypeField
          , newtypeOrigin    = Hs.NewtypeOriginTypedef typedef
          , newtypeInstances = insts
          }

        newtypeDecl :: Hs.Decl
        newtypeDecl = Hs.DeclNewtype hsNewtype

        storableDecl :: [Hs.Decl]
        storableDecl
          | Hs.Storable `Set.notMember` insts = []
          | otherwise = singleton $
              Hs.DeclDeriveInstance Hs.DeriveNewtype Hs.Storable newtypeName

        optDecls :: [Hs.Decl]
        optDecls = [
            Hs.DeclDeriveInstance strat clss newtypeName
          | (strat, clss) <- translationDeriveTypedef opts
          , clss `Set.member` insts
          ]

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

macroDecs ::
     State.MonadState InstanceMap m
  => TranslationOpts
  -> NameMangler
  -> C.MacroDecl
  -> m [Hs.Decl]
macroDecs opts nm C.MacroDecl { macroDeclMacro = m, macroDeclMacroTy = ty }
    | Macro.Quant bf <- ty
    , Macro.isPrimTy bf
    = macroDecsTypedef opts nm m

    | otherwise
    = return $ macroVarDecs nm m ty
    where

macroDecs _ _ C.MacroReparseError {} = return []
macroDecs _ _ C.MacroTcError {}      = return []

macroDecsTypedef ::
     State.MonadState InstanceMap m
  => TranslationOpts
  -> NameMangler
  -> C.Macro C.Ps
  -> m [Hs.Decl]
macroDecsTypedef opts nm macro = case C.macroBody macro of
    C.TypeMacro tyNm
      | Right ty <- C.typeNameType tyNm
      -> do
        (insts, decls) <- aux ty <$> State.get
        State.modify' $ Map.insert newtypeName insts
        return decls
    _otherwise -> return []
  where
    declPath :: C.DeclPath
    declPath = C.DeclPathName $ C.macroName macro

    newtypeName :: HsName NsTypeConstr
    newtypeName = mangle nm $ NameTycon declPath

    candidateInsts :: Set HsTypeClass
    candidateInsts = Set.union (Set.singleton Hs.Storable) $
      Set.fromList (snd <$> translationDeriveTypedef opts)

    -- everything in aux is state-dependent
    aux :: C.Type -> InstanceMap -> (Set HsTypeClass, [Hs.Decl])
    aux ty instanceMap = (insts,) $
        newtypeDecl : storableDecl ++ optDecls
      where
        fieldType :: HsType
        fieldType = typ nm ty

        insts :: Set HsTypeClass
        insts = getInstances instanceMap newtypeName candidateInsts [fieldType]

        hsNewtype :: Hs.Newtype
        hsNewtype = Hs.Newtype {
            newtypeName      = newtypeName
          , newtypeConstr    = mangle nm $ NameDatacon declPath
          , newtypeField     = Hs.Field {
                fieldName   = mangle nm $ NameDecon declPath
              , fieldType   = fieldType
              , fieldOrigin = Hs.FieldOriginNone
              }
          , newtypeOrigin    = Hs.NewtypeOriginMacro macro
          , newtypeInstances = insts
          }

        newtypeDecl :: Hs.Decl
        newtypeDecl = Hs.DeclNewtype hsNewtype

        storableDecl :: [Hs.Decl]
        storableDecl
          | Hs.Storable `Set.notMember` insts = []
          | otherwise = singleton $
              Hs.DeclDeriveInstance Hs.DeriveNewtype Hs.Storable newtypeName

        optDecls :: [Hs.Decl]
        optDecls = [
            Hs.DeclDeriveInstance strat clss newtypeName
          | (strat, clss) <- translationDeriveTypedef opts
          , clss `Set.member` insts
          ]

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data TypeContext =
    CTop     -- ^ Anything else
  | CFunArg  -- ^ Function argument
  | CFunRes  -- ^ Function result
  | CPtrArg  -- ^ Pointer argument
  deriving stock (Show)

typ :: NameMangler -> C.Type -> Hs.HsType
typ = typ' CTop

typ' :: TypeContext -> NameMangler -> C.Type -> Hs.HsType
typ' ctx nm = go ctx
  where
    go :: TypeContext -> C.Type -> Hs.HsType
    go _ (C.TypeTypedef c) =
        Hs.HsTypRef (mangle nm $ NameTycon (C.DeclPathName c)) -- wrong
    go _ (C.TypeStruct declPath) =
        Hs.HsTypRef (mangle nm $ NameTycon declPath)
    go _ (C.TypeUnion declPath) =
        -- TODO: UnionTypeConstrContext?
        Hs.HsTypRef (mangle nm $ NameTycon declPath)
    go _ (C.TypeEnum declPath) =
        Hs.HsTypRef (mangle nm $ NameTycon declPath)
    go c C.TypeVoid =
        Hs.HsPrimType (goVoid c)
    go _ (C.TypePrim p) =
        Hs.HsPrimType (goPrim p)
    go _ (C.TypePointer t) = case t of
        C.TypeFun {} -> Hs.HsFunPtr (go CPtrArg t)
        _            -> Hs.HsPtr (go CPtrArg t)
    go _ (C.TypeConstArray (C.Size n _) ty) =
        -- TODO(#460): we are dropping the size expression, only using its value.
        Hs.HsConstArray n (go CTop ty)
    go c (C.TypeIncompleteArray ty) =
        goArrayUnknownSize c ty
    go _ (C.TypeFun xs y) =
        foldr (\x res -> Hs.HsFun (go CFunArg x) res) (Hs.HsIO (go CFunRes y)) xs
    go _ (C.TypeExtBinding extId ty) =
        Hs.HsExtBinding extId ty

    goPrim :: C.PrimType -> HsPrimType
    goPrim C.PrimBool           = HsPrimCBool
    goPrim (C.PrimIntegral i s) = integralType i s
    goPrim (C.PrimFloating f)   = floatingType f
    goPrim C.PrimPtrDiff        = HsPrimCPtrDiff
    goPrim C.PrimSize           = HsPrimCSize
    goPrim (C.PrimChar sign)    =
        case sign of
          C.PrimSignImplicit _          -> HsPrimCChar
          C.PrimSignExplicit C.Signed   -> HsPrimCSChar
          C.PrimSignExplicit C.Unsigned -> HsPrimCUChar

    goVoid :: TypeContext -> HsPrimType
    goVoid CFunRes = HsPrimUnit
    goVoid CPtrArg = HsPrimVoid
    goVoid c       = panicPure $ "unexpected type void in context " ++ show c
      -- TODO: we can run into this with macros, e.g.
      --
      --   #define MyVoid void

    goArrayUnknownSize :: TypeContext -> C.Type -> HsType
    goArrayUnknownSize CFunArg t =
         -- Arrays of unknown size as function args are treated as pointers.
         -- <https://en.cppreference.com/w/c/language/array#Arrays_of_unknown_size>
         Hs.HsPtr $ go CTop t
    goArrayUnknownSize c _ =
        -- TODO <https://github.com/well-typed/hs-bindgen/issues/377>
        -- We need to extend 'TypeContext' with a context for extern
        -- declarations, and then allow for arrays of unknown size.
        panicPure $ "unexpected array of unknown size in context " ++ show c

integralType :: C.PrimIntType -> C.PrimSign -> HsPrimType
integralType C.PrimInt      C.Signed   = HsPrimCInt
integralType C.PrimInt      C.Unsigned = HsPrimCUInt
integralType C.PrimShort    C.Signed   = HsPrimCShort
integralType C.PrimShort    C.Unsigned = HsPrimCUShort
integralType C.PrimLong     C.Signed   = HsPrimCLong
integralType C.PrimLong     C.Unsigned = HsPrimCULong
integralType C.PrimLongLong C.Signed   = HsPrimCLLong
integralType C.PrimLongLong C.Unsigned = HsPrimCULLong

floatingType :: C.PrimFloatType -> HsPrimType
floatingType = \case
  C.PrimFloat      -> HsPrimCFloat
  C.PrimDouble     -> HsPrimCDouble
  C.PrimLongDouble -> throwPure_TODO 349 "long double not supported"

{-------------------------------------------------------------------------------
  Function
-------------------------------------------------------------------------------}

data WrappedType
    = WrapType C.Type
    | FancyType C.Type
  deriving Show

unwrapType :: WrappedType -> C.Type
unwrapType (WrapType ty) = ty
unwrapType (FancyType ty) = C.TypePointer ty

unwrapOrigType :: WrappedType -> C.Type
unwrapOrigType (WrapType ty) = ty
unwrapOrigType (FancyType ty) = ty

isVoidW :: WrappedType -> Bool
isVoidW = C.isVoid . unwrapType

isWrappedFancy :: WrappedType -> Bool
isWrappedFancy (WrapType _) = False
isWrappedFancy (FancyType _) = True

-- | userland-api C wrapper.
wrapperDecl
    :: String    -- ^ true C name
    -> String    -- ^ wrapper name
    -> WrappedType    -- ^ result type
    -> [WrappedType]  -- ^ arguments
    -> PC.Decl
wrapperDecl innerName wrapperName res args
    | isVoidW res
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName C.TypeVoid (unwrapType <$> args')
          [PC.Expr $ PC.Call innerName (callArgs args' (PC.argsToIdx args'))]

    | isWrappedFancy res
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName C.TypeVoid (unwrapType <$> (args' :> res))
          [PC.Assign (PC.LDeRef (PC.LVar IZ)) $ PC.Call innerName (callArgs args' (IS <$> PC.argsToIdx args'))]

    | otherwise
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName (unwrapType res) (unwrapType <$> args')
          [PC.Return $ PC.Call innerName (callArgs args' (PC.argsToIdx args'))]
  where
    callArgs :: Env ctx' WrappedType -> Env ctx' (Idx ctx) -> [PC.Expr ctx]
    callArgs tys ids = toList (zipWithEnv f tys ids) where f ty idx = if isWrappedFancy ty then PC.DeRef (PC.Var idx) else PC.Var idx

hsWrapperDecl
    :: NameMangler
    -> HsName NsVar   -- ^ haskell name
    -> HsName NsVar   -- ^ low-level import name
    -> WrappedType    -- ^ result type
    -> [WrappedType]  -- ^ arguments
    -> SHs.SDecl
hsWrapperDecl nm hiName loName res args
  | isWrappedFancy res
  = SHs.DVar
    hiName
    (Just (SHs.translateType hsty))
    (goA EmptyEnv args)

  | otherwise
  = SHs.DVar
    hiName
    (Just (SHs.translateType hsty))
    (goB EmptyEnv args)
  where
    hsty = foldr HsFun (HsIO $ typ' CFunRes nm $ unwrapOrigType res) (typ' CFunArg nm . unwrapOrigType <$> args)

    -- wrapper for fancy result
    goA :: Env ctx WrappedType -> [WrappedType] -> SHs.SExpr ctx
    goA env []     = goA' env (tabulateEnv (sizeEnv env) id) []
    goA env (x:xs) = SHs.ELam "x" $ goA (env :> x) xs

    goA' :: Env ctx' WrappedType -> Env ctx' (Idx ctx) -> [Idx ctx] -> SHs.SExpr ctx
    goA' EmptyEnv    EmptyEnv  zs
        = shsApps (SHs.EGlobal SHs.CAPI_allocaAndPeek)
          [ SHs.ELam "z" $ shsApps (SHs.EFree loName) (map SHs.EBound (fmap IS zs ++ [IZ]))
          ]

    goA' (tys :> ty) (xs :> x) zs
        | isWrappedFancy ty
        = shsApps (SHs.EGlobal SHs.CAPI_with)
          [ SHs.EBound x
          , SHs.ELam "y" $ goA' tys (IS <$> xs) (IZ : fmap IS zs)
          ]

        | otherwise
        = goA' tys xs (x : zs)

    -- wrapper for non-fancy result.
    goB :: Env ctx WrappedType -> [WrappedType] -> SHs.SExpr ctx
    goB env []     = goB' env (tabulateEnv (sizeEnv env) id) []
    goB env (x:xs) = SHs.ELam "x" $ goB (env :> x) xs

    goB' :: Env ctx' WrappedType -> Env ctx' (Idx ctx) -> [Idx ctx] -> SHs.SExpr ctx
    goB' EmptyEnv    EmptyEnv  zs
        = shsApps (SHs.EFree loName) (map SHs.EBound zs)

    goB' (tys :> ty) (xs :> x) zs
        | isWrappedFancy ty
        = shsApps (SHs.EGlobal SHs.CAPI_with)
          [ SHs.EBound x
          , SHs.ELam "y" $ goB' tys (IS <$> xs) (IZ : fmap IS zs)
          ]

        | otherwise
        = goB' tys xs (x : zs)

shsApps :: SHs.SExpr ctx -> [SHs.SExpr ctx] -> SHs.SExpr ctx
shsApps = foldl' SHs.EApp

functionDecs ::
     NameMangler
  -> ModuleUnique
  -> Map C.CName C.Type -- ^ typedefs
  -> C.Function
  -> [Hs.Decl]
functionDecs nm mu typedefs f =
    [ Hs.DeclInlineCInclude $ getCHeaderIncludePath $ C.functionHeader f
    , Hs.DeclInlineC $ PC.prettyDecl (wrapperDecl innerName wrapperName res args) ""
    , Hs.DeclForeignImport $ Hs.ForeignImportDecl
        { foreignImportName       = importName
        , foreignImportType       = importType
        , foreignImportOrigName   = T.pack wrapperName
        , foreignImportHeader     = getCHeaderIncludePath $ C.functionHeader f
        , foreignImportDeclOrigin = Hs.ForeignImportDeclOriginFunction f
        }
    ] ++
    -- TODO: Hs wrapper if any type is fancy
    [ Hs.DeclSimple $ hsWrapperDecl nm highlevelName importName res args
    | anyFancy
    ]
  where
    anyFancy = any isWrappedFancy (res : args)

    highlevelName = mangle nm $ NameVar $ C.functionName f
    importName
        | anyFancy   = mangle nm $ NameLowLevelVar $ C.functionName f
        | otherwise  = highlevelName

    wrapType :: C.Type -> WrappedType
    wrapType ty
        | isFancy ty = FancyType ty
        | otherwise = WrapType ty

    res = wrapType $ C.functionRes f
    args = wrapType <$> C.functionArgs f

    -- types which we cannot pass directly using C FFI.
    isFancy :: C.Type -> Bool
    isFancy C.TypeStruct {}     = True
    isFancy C.TypeUnion {}      = True
    isFancy C.TypeConstArray {} = True
    isFancy (C.TypeTypedef n)   =
        let t = Map.findWithDefault (panicPure $ "Unbound typedef " ++ show n) n typedefs
        in isFancy t
    isFancy _ = False

    importType :: HsType
    importType
        | isWrappedFancy res
        = foldr HsFun (HsIO $ typ' CFunRes nm C.TypeVoid) (typ' CFunArg nm . unwrapType <$> (args ++ [res]))

        | otherwise
        = foldr HsFun (HsIO $ typ' CFunRes nm $ unwrapType res) (typ' CFunArg nm . unwrapType <$> args)

    -- below is generation of C wrapper for userland-capi.
    innerName :: String
    innerName = T.unpack (C.getCName (C.functionName f))

    wrapperName :: String
    wrapperName = unModuleUnique mu ++ "_" ++ innerName

{-------------------------------------------------------------------------------
  Macro
-------------------------------------------------------------------------------}

macroVarDecs ::
     NameMangler
  -> C.Macro p
  -> Macro.Quant ( Macro.Type Macro.Ty )
  -> [Hs.Decl]
macroVarDecs nm (C.Macro { macroName = cVarNm, macroArgs = args, macroBody = body } ) qty =
  [
    Hs.DeclVar $
      Hs.VarDecl
        { varDeclName = hsVarName
        , varDeclType = quantTyHsTy qty
        , varDeclBody = hsBody
        }
  | hsBody <- toList $ macroLamHsExpr nm cVarNm args body
  ]
  where
    hsVarName = mangle nm $ NameVar cVarNm

quantTyHsTy :: Macro.Quant ( Macro.Type Macro.Ty ) -> Hs.SigmaType
quantTyHsTy qty@(Macro.Quant @kis _) =
  case Macro.mkQuantTyBody qty of
    Macro.QuantTyBody { quantTyQuant = cts, quantTyBody = ty } -> do
      goForallTy (Macro.tyVarNames @kis) cts ty
  where

    goCt :: Map Text (Idx ctx) -> Macro.Type Macro.Ct -> Hs.PredType ctx
    goCt env (Macro.TyConAppTy cls as) =
      Hs.DictTy (Hs.AClass cls) (goTys env as)
    goCt env (Macro.NomEqPred a b) =
      Hs.NomEqTy (goTy env a) (goTy env b)

    goTy :: Map Text (Idx ctx) -> Macro.Type Macro.Ty -> Hs.TauType ctx
    goTy env (Macro.TyVarTy tv) = Hs.TyVarTy (env Map.! Macro.tyVarName tv) -- XXX: partial Map.!
    goTy env (Macro.FunTy as r) =
      foldr (Hs.FunTy . goTy env) (goTy env r) as
    goTy env (Macro.TyConAppTy tc as) =
      Hs.TyConAppTy (Hs.ATyCon tc) (goTys env as)

    goTys :: Map Text (Idx ctx) -> Vec n ( Macro.Type Macro.Ty ) -> [ Hs.TauType ctx ]
    goTys env as = toList $ fmap (goTy env) as

    goForallTy :: forall n. SNatI n => Vec n (Int, Text) -> [ Macro.Type Macro.Ct ] -> Macro.Type Macro.Ty -> Hs.SigmaType
    goForallTy args cts body =
        let
          env :: Map Text (Idx n)
          env = Map.fromList $ toList $ Vec.zipWith (,) ( fmap snd args ) qtvs
          qtvs :: Vec n (Idx n)
          qtvs = unU (induction (U VNil) (\(U v) -> U (IZ ::: fmap IS v)))
        in
          Hs.ForallTy
            { forallTyBinders = fmap (fromString . T.unpack . snd) args
            , forallTy        = Hs.QuantTy
                { quantTyCts  = fmap (goCt env) cts
                , quantTyBody = goTy env body
                }
            }

newtype U n = U { unU :: Vec n (Idx n) }

macroLamHsExpr ::
     NameMangler
  -> C.CName
  -> [C.CName]
  -> C.MacroBody p
  -> Maybe (Hs.VarDeclRHS EmptyCtx)
macroLamHsExpr nm _macroName macroArgs body =
  case body of
    C.EmptyMacro -> Nothing
    C.AttributeMacro {} -> Nothing
    C.TypeMacro {} -> Nothing
    C.ExpressionMacro expr ->
      makeNames macroArgs Map.empty
      where
        makeNames :: [C.CName] -> Map C.CName (Idx ctx) -> Maybe (Hs.VarDeclRHS ctx)
        makeNames []     env = macroExprHsExpr nm env expr
        makeNames (n:ns) env = Hs.VarDeclLambda . Hs.Lambda (cnameToHint n) <$> makeNames ns (Map.insert n IZ (fmap IS env))

cnameToHint :: C.CName -> NameHint
cnameToHint (C.CName t) = fromString (T.unpack t)

macroExprHsExpr ::
     NameMangler
  -> Map C.CName (Idx ctx)
  -> C.MExpr p
  -> Maybe (Hs.VarDeclRHS ctx)
macroExprHsExpr nm = goExpr where
    goExpr :: Map C.CName (Idx ctx) -> C.MExpr p -> Maybe (Hs.VarDeclRHS ctx)
    goExpr env = \case
      C.MTerm tm -> goTerm env tm
      C.MApp _xapp fun args ->
        goApp env (Hs.InfixAppHead fun) (toList args)

    goTerm :: Map C.CName (Idx ctx) -> C.MTerm p -> Maybe (Hs.VarDeclRHS ctx)
    goTerm env = \case
      C.MInt i -> goInt i
      C.MFloat f -> goFloat f
      C.MChar c -> goChar c
      C.MString s -> goString s
      C.MVar _xvar cname args ->
        --  TODO: removed the macro argument used as a function check.
        case Map.lookup cname env of
          Just i  -> return (Hs.VarDeclVar i)
          Nothing ->
            let hsVar = mangle nm $ NameVar cname
            in  goApp env (Hs.VarAppHead hsVar) args
      C.MStringize {} -> Nothing
      C.MConcat {} -> Nothing

    goApp :: Map C.CName (Idx ctx) -> Hs.VarDeclRHSAppHead -> [C.MExpr p] -> Maybe (Hs.VarDeclRHS ctx)
    goApp env appHead args = do
      args' <- traverse (goExpr env) args
      return $ Hs.VarDeclApp appHead args'

    goInt :: C.IntegerLiteral -> Maybe (Hs.VarDeclRHS ctx)
    goInt (C.IntegerLiteral { integerLiteralType = intyTy, integerLiteralValue = i }) =
      Just $ Hs.VarDeclIntegral i $
        hsPrimIntTy $ C.Type.IntLike intyTy

    goChar :: C.CharLiteral -> Maybe (Hs.VarDeclRHS ctx)
    goChar (C.CharLiteral { charLiteralValue = c }) =
      return $ Hs.VarDeclChar c

    goString :: C.StringLiteral -> Maybe (Hs.VarDeclRHS ctx)
    goString (C.StringLiteral { stringLiteralValue = s }) = do
      let bytes = concatMap (IsList.toList . C.Char.charValue) s
      return $
        Hs.VarDeclString (IsList.fromList bytes)

    goFloat :: C.FloatingLiteral -> Maybe (Hs.VarDeclRHS ctx)
    goFloat flt@(C.FloatingLiteral { floatingLiteralType = fty }) =
      case fty of
        C.Type.FloatType  -> Just $ Hs.VarDeclFloat (C.floatingLiteralFloatValue flt)
        C.Type.DoubleType -> Just $ Hs.VarDeclDouble (C.floatingLiteralDoubleValue flt)
