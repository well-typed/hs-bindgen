{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Low-level translation of the C header to a Haskell module
module HsBindgen.Hs.Translation (
    TranslationOpts(..)
  , generateDeclarations
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
import C.Type qualified (FloatingType (..), IntegralType (IntLike))
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config.FixCandidate (FixCandidate)
import HsBindgen.Config.FixCandidate qualified as FixCandidate
import HsBindgen.Errors
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.Macro.Tc qualified as Macro
import HsBindgen.Frontend.RootHeader (getHashIncludeArg)
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Type
import HsBindgen.Hs.CallConv
import HsBindgen.Hs.Haddock.Documentation qualified as Hs
import HsBindgen.Hs.Origin qualified as Origin
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell
import HsBindgen.ModuleUnique
import HsBindgen.NameHint
import HsBindgen.PrettyC qualified as PC
import HsBindgen.SHs.AST qualified as SHs
import HsBindgen.SHs.Translation qualified as SHs

import DeBruijn (Add (..), EmptyCtx, Env (..), Idx (..), pattern I1, pattern I2,
                 sizeEnv, tabulateEnv, weaken, zipWithEnv)
import HsBindgen.Hs.Haddock.Translation (generateHaddocks)

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

instance Default TranslationOpts where
  def = TranslationOpts {
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

generateDeclarations ::
     TranslationOpts
  -> ModuleUnique
  -> [C.Decl]
  -> [Hs.Decl]
generateDeclarations opts mu decs =
    flip State.evalState Map.empty $
      concat <$> mapM (generateDecs opts mu typedefs) decs
  where
    typedefs :: Map C.Name C.Type
    typedefs = Map.union actualTypedefs pseudoTypedefs

    -- typedef lookup table
    -- shallow: only one layer of typedefs is stripped.
    actualTypedefs :: Map C.Name C.Type
    actualTypedefs = Map.fromList
        [ (C.nameC (C.declId declInfo), typedefType)
        | C.Decl{declInfo, declKind} <- decs
        , C.DeclTypedef typedef <- [declKind]
        , let C.Typedef{typedefType} = typedef
        ]

    -- macros also act as "typedef"s
    pseudoTypedefs :: Map C.Name C.Type
    pseudoTypedefs = Map.fromList
        [ (C.nameC (C.declId declInfo), macroType)
        | C.Decl{declInfo, declKind} <- decs
        , C.DeclMacro macro <- [declKind]
        , C.MacroType C.CheckedMacroType{macroType} <- [macro]
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
          HsIncompleteArray hsType' ->
            -- constrain by Array item type in next step
            aux (acc /\ arrayInsts) $ hsType' : hsTypes
          HsPtr{} -> aux (acc /\ ptrInsts) hsTypes
          HsFunPtr{} -> aux (acc /\ ptrInsts) hsTypes
          HsIO{} -> Set.empty
          HsFun{} -> Set.empty
          HsExtBinding _ref typeSpec ->
            let acc' = acc /\ typeSpecInsts typeSpec
            in  aux acc' hsTypes
          HsByteArray{} ->
            let acc' = acc /\ Set.fromList [Hs.Eq, Hs.Ord, Hs.Show]
            in  aux acc' hsTypes
          HsSizedByteArray{} ->
            let acc' = acc /\ Set.fromList [Hs.Eq, Hs.Show]
            in  aux acc' hsTypes
          HsBlock t ->
            aux acc (t:hsTypes)

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

    arrayInsts :: Set HsTypeClass
    arrayInsts = Set.fromList [
        Hs.Eq
      , Hs.Show
      ]

    typeSpecInsts :: BindingSpec.TypeSpec -> Set HsTypeClass
    typeSpecInsts typeSpec = Set.fromAscList [
        cls
      | (cls, BindingSpec.Require{}) <-
           Map.toAscList (BindingSpec.typeSpecInstances typeSpec)
      ]

{-------------------------------------------------------------------------------
  Declarations
------------------------------------------------------------------------------}

-- TODO: Take DeclSpec into account
generateDecs ::
     State.MonadState InstanceMap m
  => TranslationOpts
  -> ModuleUnique
  -> Map C.Name C.Type
  -> C.Decl
  -> m [Hs.Decl]
generateDecs opts mu typedefs (C.Decl info kind spec) =
    case kind of
      C.DeclStruct struct ->
        reifyStructFields struct $ structDecs opts info struct spec
      C.DeclStructOpaque ->
        opaqueStructDecs info spec
      C.DeclUnion union ->
        unionDecs info union spec
      C.DeclUnionOpaque ->
        opaqueUnionDecs info spec
      C.DeclEnum e ->
        enumDecs opts info e spec
      C.DeclEnumOpaque ->
        opaqueEnumDecs info spec
      C.DeclTypedef d ->
        typedefDecs opts info d spec
      C.DeclFunction f ->
        return $ functionDecs mu typedefs info f spec
      C.DeclMacro macro ->
        macroDecs opts info macro spec
      C.DeclGlobal ty ->
        return $ global info ty spec
      C.DeclConst ty ->
        return $ globalConst info ty spec

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
  -> C.DeclInfo
  -> C.Struct
  -> C.DeclSpec
  -> Vec n C.StructField
  -> m [Hs.Decl]
structDecs opts info struct spec fields = do
    (insts, decls) <- aux <$> State.get
    State.modify' $ Map.insert structName insts
    return decls
  where
    structName :: HsName NsTypeConstr
    structName = C.nameHs (C.declId info)

    structFields :: Vec n Hs.Field
    structFields = flip Vec.map fields $ \f -> Hs.Field {
        fieldName    = C.nameHs (C.structFieldName f)
      , fieldType    = typ (C.structFieldType f)
      , fieldOrigin  = Origin.StructField f
      , fieldComment = fmap generateHaddocks (C.structFieldComment f)
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
          , structConstr    = C.recordConstr (C.structNames struct)
          , structFields    = structFields
          , structInstances = insts
          , structOrigin    = Just Origin.Decl{
                declInfo = info
              , declKind = Origin.Struct struct
              , declSpec = spec
              }
          , structComment = fmap generateHaddocks (C.declComment info)
          }

        structDecl :: Hs.Decl
        structDecl = Hs.DeclData hsStruct

        storableDecl :: [Hs.Decl]
        storableDecl
          | Hs.Storable `Set.notMember` insts = []
          | otherwise = singleton
                      $ Hs.DeclDefineInstance
                          Hs.DefineInstance {
                            defineInstanceComment      = Nothing
                          , defineInstanceDeclarations =
                              Hs.InstanceStorable
                                  hsStruct
                                  Hs.StorableInstance {
                                    Hs.storableSizeOf    = C.structSizeof struct
                                  , Hs.storableAlignment = C.structAlignment struct
                                  , Hs.storablePeek      = Hs.Lambda "ptr" $
                                      Hs.Ap (Hs.StructCon hsStruct) $
                                        map (peekStructField IZ) (C.structFields struct)
                                  , Hs.storablePoke      = Hs.Lambda "ptr" $ Hs.Lambda "s" $
                                      Hs.makeElimStruct IZ hsStruct $ \wk xs -> Hs.Seq $ toList $
                                        Vec.zipWith (pokeStructField (weaken wk I1)) fields xs
                                  }
                          }

        optDecls :: [Hs.Decl]
        optDecls = [
            Hs.DeclDeriveInstance
              Hs.DeriveInstance {
                deriveInstanceStrategy = strat
              , deriveInstanceClass    = clss
              , deriveInstanceName     = structName
              , deriveInstanceComment  = Nothing
              }
          | (strat, clss) <- translationDeriveStruct opts
          , clss `Set.member` insts
          ]

        hasFlamDecl :: [Hs.Decl]
        hasFlamDecl = case C.structFlam struct of
          Nothing   -> []
          Just flam -> singleton
                     $ Hs.DeclDefineInstance
                        Hs.DefineInstance {
                          defineInstanceComment      = Nothing
                        , defineInstanceDeclarations =
                            Hs.InstanceHasFLAM hsStruct
                                               (typ (C.structFieldType flam))
                                               (C.structFieldOffset flam `div` 8)
                        }

peekStructField :: Idx ctx -> C.StructField -> Hs.PeekByteOff ctx
peekStructField ptr f = case C.structFieldWidth f of
    Nothing -> Hs.PeekByteOff ptr (C.structFieldOffset f `div` 8)
    Just w  -> Hs.PeekBitOffWidth ptr (C.structFieldOffset f) w

pokeStructField :: Idx ctx -> C.StructField -> Idx ctx -> Hs.PokeByteOff ctx
pokeStructField ptr f i = case C.structFieldWidth f of
    Nothing -> Hs.PokeByteOff ptr (C.structFieldOffset f `div` 8) i
    Just w  -> Hs.PokeBitOffWidth ptr (C.structFieldOffset f) w i

{-------------------------------------------------------------------------------
  Opaque struct and opaque enum
-------------------------------------------------------------------------------}

opaqueDecs ::
     State.MonadState InstanceMap m
  => Origin.EmptyData
  -> C.DeclInfo
  -> C.DeclSpec
  -> m [Hs.Decl]
opaqueDecs origin info spec = do
    State.modify' $ Map.insert name Set.empty
    return [decl]
  where
    name :: HsName NsTypeConstr
    name = C.nameHs (C.declId info)

    decl :: Hs.Decl
    decl = Hs.DeclEmpty Hs.EmptyData {
        emptyDataName   = name
      , emptyDataOrigin = Origin.Decl{
            declInfo = info
          , declKind = origin
          , declSpec = spec
          }
      , emptyDataComment = fmap generateHaddocks (C.declComment info)
      }

opaqueStructDecs ::
     State.MonadState InstanceMap m
  => C.DeclInfo
  -> C.DeclSpec
  -> m [Hs.Decl]
opaqueStructDecs = opaqueDecs Origin.OpaqueStruct

opaqueEnumDecs ::
     State.MonadState InstanceMap m
  => C.DeclInfo
  -> C.DeclSpec
  -> m [Hs.Decl]
opaqueEnumDecs = opaqueDecs Origin.OpaqueEnum

opaqueUnionDecs ::
     State.MonadState InstanceMap m
  => C.DeclInfo
  -> C.DeclSpec
  -> m [Hs.Decl]
opaqueUnionDecs = opaqueDecs Origin.OpaqueUnion

{-------------------------------------------------------------------------------
  Unions
-------------------------------------------------------------------------------}

unionDecs ::
     State.MonadState InstanceMap m
  => C.DeclInfo
  -> C.Union
  -> C.DeclSpec
  -> m [Hs.Decl]
unionDecs info union spec = do
    decls <- aux <$> State.get
    State.modify' $ Map.insert newtypeName insts
    return decls
  where
    newtypeName :: HsName NsTypeConstr
    newtypeName = C.nameHs (C.declId info)

    insts :: Set HsTypeClass
    insts = Set.singleton Hs.Storable

    hsNewtype :: Hs.Newtype
    hsNewtype = Hs.Newtype {
        newtypeName      = newtypeName
      , newtypeConstr    = C.newtypeConstr (C.unionNames union)
      , newtypeInstances = insts

      , newtypeField = Hs.Field {
            fieldName    = C.newtypeField (C.unionNames union)
          , fieldType    = Hs.HsByteArray
          , fieldOrigin  = Origin.GeneratedField
          , fieldComment = Nothing
          }
      , newtypeOrigin = Origin.Decl{
            declInfo = info
          , declKind = Origin.Union union
          , declSpec = spec
          }
      , newtypeComment = fmap generateHaddocks (C.declComment info)
      }

    newtypeDecl :: Hs.Decl
    newtypeDecl = Hs.DeclNewtype hsNewtype

    storableDecl :: Hs.Decl
    storableDecl =
      Hs.DeclDeriveInstance
        Hs.DeriveInstance {
          deriveInstanceStrategy = Hs.DeriveVia sba
        , deriveInstanceClass    = Hs.Storable
        , deriveInstanceName     = newtypeName
        , deriveInstanceComment  = Nothing
        }

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

        -- TODO: Should the name mangler take care of the "get" and "set" prefixes?
        getAccessorDecls :: C.UnionField -> [Hs.Decl]
        getAccessorDecls C.UnionField{..} =
          let hsType = typ unionFieldType
              fInsts = getInstances instanceMap newtypeName insts [hsType]
              getterName = "get_" <> C.nameHs unionFieldName
              setterName = "set_" <> C.nameHs unionFieldName
              commentRefName name = Just
                Hs.Comment {
                  commentTitle    = Nothing
                , commentOrigin   = Nothing
                , commentChildren = [ Hs.Paragraph
                                      [ Hs.Bold [Hs.TextContent "See:"]
                                      , Hs.Identifier name
                                      ]
                                    ]
                }
          in  if Hs.Storable `Set.notMember` fInsts
                then []
                else
                  [ Hs.DeclUnionGetter
                      Hs.UnionGetter {
                        unionGetterName    = getterName
                      , unionGetterType    = hsType
                      , unionGetterConstr  = newtypeName
                      , unionGetterComment = fmap generateHaddocks unionFieldComment
                                          <> commentRefName (getHsName setterName)
                      }
                  , Hs.DeclUnionSetter
                      Hs.UnionSetter {
                        unionSetterName    = setterName
                      , unionSetterType    = hsType
                      , unionSetterConstr  = newtypeName
                      , unionSetterComment = commentRefName (getHsName getterName)
                      }
                  ]

{-------------------------------------------------------------------------------
  Enum
-------------------------------------------------------------------------------}

enumDecs ::
     State.MonadState InstanceMap m
  => TranslationOpts
  -> C.DeclInfo
  -> C.Enum
  -> C.DeclSpec
  -> m [Hs.Decl]
enumDecs opts info e spec = do
    State.modify' $ Map.insert newtypeName insts
    return $
      newtypeDecl : storableDecl : optDecls ++ cEnumInstanceDecls ++ valueDecls
  where
    newtypeName :: HsName NsTypeConstr
    newtypeName = C.nameHs (C.declId info)

    newtypeConstr :: HsName NsConstr
    newtypeConstr = C.newtypeConstr (C.enumNames e)

    newtypeField :: Hs.Field
    newtypeField = Hs.Field {
        fieldName    = C.newtypeField (C.enumNames e)
      , fieldType    = typ (C.enumType e)
      , fieldOrigin  = Origin.GeneratedField
      , fieldComment = Nothing
      }

    insts :: Set HsTypeClass
    insts = Set.union (Set.fromList [Hs.Show, Hs.Read, Hs.Storable]) $
      Set.fromList (snd <$> translationDeriveEnum opts)

    hsNewtype :: Hs.Newtype
    hsNewtype = Hs.Newtype {
        newtypeName      = newtypeName
      , newtypeConstr    = newtypeConstr
      , newtypeField     = newtypeField
      , newtypeInstances = insts
      , newtypeOrigin    = Origin.Decl{
            declInfo = info
          , declKind = Origin.Enum e
          , declSpec = spec
          }
      , newtypeComment = fmap generateHaddocks (C.declComment info)
      }

    newtypeDecl :: Hs.Decl
    newtypeDecl = Hs.DeclNewtype hsNewtype

    hsStruct :: Hs.Struct (S Z)
    hsStruct = Hs.Struct {
        structName      = newtypeName
      , structConstr    = newtypeConstr
      , structFields    = Vec.singleton newtypeField
      , structInstances = insts
      , structOrigin    = Nothing
      , structComment   = Nothing
      }

    storableDecl :: Hs.Decl
    storableDecl = Hs.DeclDefineInstance
      Hs.DefineInstance {
        defineInstanceComment      = Nothing
      , defineInstanceDeclarations =
          Hs.InstanceStorable hsStruct Hs.StorableInstance {
              Hs.storableSizeOf    = C.enumSizeof e
            , Hs.storableAlignment = C.enumAlignment e
            , Hs.storablePeek      = Hs.Lambda "ptr" $
                Hs.Ap (Hs.StructCon hsStruct) [ Hs.PeekByteOff IZ 0 ]
            , Hs.storablePoke      = Hs.Lambda "ptr" $ Hs.Lambda "s" $
                Hs.ElimStruct IZ hsStruct (AS AZ) $
                  Hs.Seq [ Hs.PokeByteOff I2 0 IZ ]
            }
      }

    optDecls :: [Hs.Decl]
    optDecls = [
        Hs.DeclDeriveInstance
          Hs.DeriveInstance {
            deriveInstanceName     = newtypeName
          , deriveInstanceClass    = clss
          , deriveInstanceStrategy = strat
          , deriveInstanceComment  = Nothing
          }
      | (strat, clss) <- translationDeriveEnum opts
      ]

    valueDecls :: [Hs.Decl]
    valueDecls =
        [ Hs.DeclPatSyn Hs.PatSyn
          { patSynName    = C.nameHs enumConstantName
          , patSynType    = newtypeName
          , patSynConstr  = newtypeConstr
          , patSynValue   = enumConstantValue
          , patSynOrigin  = Origin.EnumConstant enumValue
          , patSynComment = fmap generateHaddocks enumConstantComment
          }
        | enumValue@C.EnumConstant{..} <- C.enumConstants e
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
          cEnumDecl = Hs.DeclDefineInstance
            Hs.DefineInstance {
              defineInstanceComment      = Nothing
            , defineInstanceDeclarations = Hs.InstanceCEnum hsStruct fTyp vStrs (isJust mSeqBounds)
            }
          cEnumShowDecl = Hs.DeclDefineInstance
            Hs.DefineInstance {
              defineInstanceComment      = Nothing
            , defineInstanceDeclarations = Hs.InstanceCEnumShow hsStruct
            }
          cEnumReadDecl = Hs.DeclDefineInstance
            Hs.DefineInstance {
              defineInstanceComment      = Nothing
            , defineInstanceDeclarations = Hs.InstanceCEnumRead hsStruct
            }
          sequentialCEnumDecl = case mSeqBounds of
            Just (nameMin, nameMax) -> List.singleton
                                     . Hs.DeclDefineInstance
                                     $ Hs.DefineInstance {
                                         defineInstanceComment      = Nothing
                                       , defineInstanceDeclarations =
                                           Hs.InstanceSequentialCEnum hsStruct nameMin nameMax
                                       }
            Nothing -> []
      in  cEnumDecl : sequentialCEnumDecl ++ [cEnumShowDecl, cEnumReadDecl]

{-------------------------------------------------------------------------------
  Typedef
-------------------------------------------------------------------------------}

typedefDecs ::
     State.MonadState InstanceMap m
  => TranslationOpts
  -> C.DeclInfo
  -> C.Typedef
  -> C.DeclSpec
  -> m [Hs.Decl]
typedefDecs opts info typedef spec = do
    (insts, decls) <- aux <$> State.get
    State.modify' $ Map.insert newtypeName insts
    return decls
  where
    newtypeName :: HsName NsTypeConstr
    newtypeName = C.nameHs (C.declId info)

    newtypeField :: Hs.Field
    newtypeField = Hs.Field {
        fieldName    = C.newtypeField (C.typedefNames typedef)
      , fieldType    = typ (C.typedefType typedef)
      , fieldOrigin  = Origin.GeneratedField
      , fieldComment = Nothing
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
          , newtypeConstr    = C.newtypeConstr (C.typedefNames typedef)
          , newtypeField     = newtypeField
          , newtypeInstances = insts
          , newtypeOrigin    = Origin.Decl{
                declInfo = info
              , declKind = Origin.Typedef typedef
              , declSpec = spec
              }
          , newtypeComment = fmap generateHaddocks (C.declComment info)
          }

        newtypeDecl :: Hs.Decl
        newtypeDecl = Hs.DeclNewtype hsNewtype

        storableDecl :: [Hs.Decl]
        storableDecl
          | Hs.Storable `Set.notMember` insts = []
          | otherwise = singleton $
              Hs.DeclDeriveInstance
                Hs.DeriveInstance {
                  deriveInstanceStrategy = Hs.DeriveNewtype
                , deriveInstanceClass    = Hs.Storable
                , deriveInstanceName     = newtypeName
                , deriveInstanceComment  = Nothing
                }

        optDecls :: [Hs.Decl]
        optDecls = [
            Hs.DeclDeriveInstance
              Hs.DeriveInstance {
                deriveInstanceStrategy = strat
              , deriveInstanceClass    = clss
              , deriveInstanceName     = newtypeName
              , deriveInstanceComment  = Nothing
              }
          | (strat, clss) <- translationDeriveTypedef opts
          , clss `Set.member` insts
          ]

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

macroDecs ::
     State.MonadState InstanceMap m
  => TranslationOpts
  -> C.DeclInfo
  -> C.CheckedMacro
  -> C.DeclSpec
  -> m [Hs.Decl]
macroDecs opts info checkedMacro spec =
    case checkedMacro of
      C.MacroType ty   -> macroDecsTypedef opts info ty spec
      C.MacroExpr expr -> return $ macroVarDecs info expr

macroDecsTypedef ::
     State.MonadState InstanceMap m
  => TranslationOpts
  -> C.DeclInfo
  -> C.CheckedMacroType
  -> C.DeclSpec
  -> m [Hs.Decl]
macroDecsTypedef opts info macroType spec = do
    (insts, decls) <- aux (C.macroType macroType) <$> State.get
    State.modify' $ Map.insert newtypeName insts
    return decls
  where
    newtypeName :: HsName NsTypeConstr
    newtypeName = C.nameHs (C.declId info)

    candidateInsts :: Set HsTypeClass
    candidateInsts = Set.union (Set.singleton Hs.Storable) $
      Set.fromList (snd <$> translationDeriveTypedef opts)

    -- everything in aux is state-dependent
    aux :: C.Type -> InstanceMap -> (Set HsTypeClass, [Hs.Decl])
    aux ty instanceMap = (insts,) $
        newtypeDecl : storableDecl ++ optDecls
      where
        fieldType :: HsType
        fieldType = typ ty

        insts :: Set HsTypeClass
        insts = getInstances instanceMap newtypeName candidateInsts [fieldType]

        hsNewtype :: Hs.Newtype
        hsNewtype = Hs.Newtype {
            newtypeName      = newtypeName
          , newtypeConstr    = C.newtypeConstr (C.macroTypeNames macroType)
          , newtypeInstances = insts

          , newtypeField = Hs.Field {
                fieldName    = C.newtypeField (C.macroTypeNames macroType)
              , fieldType    = fieldType
              , fieldOrigin  = Origin.GeneratedField
              , fieldComment = Nothing
              }
          , newtypeOrigin = Origin.Decl {
                declInfo = info
              , declKind = Origin.Macro macroType
              , declSpec = spec
              }
          , newtypeComment = fmap generateHaddocks (C.declComment info)
          }

        newtypeDecl :: Hs.Decl
        newtypeDecl = Hs.DeclNewtype hsNewtype

        storableDecl :: [Hs.Decl]
        storableDecl
          | Hs.Storable `Set.notMember` insts = []
          | otherwise = singleton $
              Hs.DeclDeriveInstance
                Hs.DeriveInstance {
                  deriveInstanceStrategy = Hs.DeriveNewtype
                , deriveInstanceClass    = Hs.Storable
                , deriveInstanceName     = newtypeName
                , deriveInstanceComment  = Nothing
                }

        optDecls :: [Hs.Decl]
        optDecls = [
            Hs.DeclDeriveInstance
              Hs.DeriveInstance {
                deriveInstanceStrategy = strat
              , deriveInstanceClass    = clss
              , deriveInstanceName     = newtypeName
              , deriveInstanceComment  = Nothing
              }
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

typ :: HasCallStack => C.Type -> Hs.HsType
typ = typ' CTop

typ' :: HasCallStack => TypeContext -> C.Type -> Hs.HsType
typ' ctx = go ctx
  where
    go :: TypeContext -> C.Type -> Hs.HsType
    go _ (C.TypeTypedef (C.TypedefRegular name)) =
        Hs.HsTypRef (C.nameHs name)
    go c (C.TypeTypedef (C.TypedefSquashed _name ty)) =
        go c ty
    go _ (C.TypeStruct name _origin) =
        Hs.HsTypRef (C.nameHs name)
    go _ (C.TypeUnion name _origin) =
        Hs.HsTypRef (C.nameHs name)
    go _ (C.TypeEnum name _origin) =
        Hs.HsTypRef (C.nameHs name)
    go _ (C.TypeMacroTypedef name _origin) =
        Hs.HsTypRef (C.nameHs name)
    go c C.TypeVoid =
        Hs.HsPrimType (goVoid c)
    go _ (C.TypePrim p) =
        Hs.HsPrimType (goPrim p)
    go _ (C.TypePointer t) = case t of
        C.TypeFun {} -> Hs.HsFunPtr (go CPtrArg t)
        _            -> Hs.HsPtr (go CPtrArg t)
    go _ (C.TypeConstArray n ty) =
        Hs.HsConstArray n $ go CTop ty
    go _ (C.TypeIncompleteArray ty) =
        Hs.HsIncompleteArray $ go CTop ty
    go _ (C.TypeFun xs y) =
        foldr (\x res -> Hs.HsFun (go CFunArg x) res) (Hs.HsIO (go CFunRes y)) xs
    go _ (C.TypeBlock ty) =
        HsBlock $ go CTop ty
    go _ (C.TypeExtBinding ext) =
        Hs.HsExtBinding (C.extHsRef ext) (C.extHsSpec ext)

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
  C.PrimFloat  -> HsPrimCFloat
  C.PrimDouble -> HsPrimCDouble

{-------------------------------------------------------------------------------
  Function
-------------------------------------------------------------------------------}

data WrappedType
    = WrapType C.Type -- ^ ordinary, "primitive" types which can be handled by Haskell FFI directly
    | HeapType C.Type -- ^ types passed on heap
    | CAType C.Type Natural C.Type -- ^ constant arrays. The C ABI is to pass these as pointers, but we need a wrapper on Haskell side.
    | AType C.Type C.Type
  deriving Show

-- | Type in low-level Haskell wrapper
unwrapType :: WrappedType -> C.Type
unwrapType (WrapType ty)   = ty
unwrapType (HeapType ty)   = C.TypePointer ty
unwrapType (CAType _ _ ty) = C.TypePointer ty
unwrapType (AType _ ty)    = C.TypePointer ty

-- | Type in high-level Haskell wrapper
unwrapOrigType :: WrappedType -> C.Type
unwrapOrigType (WrapType ty)    = ty
unwrapOrigType (HeapType ty)    = ty
unwrapOrigType (CAType oty _ _) = oty
unwrapOrigType (AType oty _)    = oty

isVoidW :: WrappedType -> Bool
isVoidW = C.isVoid . unwrapType

-- | Whether wrapped type is HeapType.
isWrappedHeap :: WrappedType -> Bool
isWrappedHeap WrapType {} = False
isWrappedHeap HeapType {} = True
isWrappedHeap CAType {}   = False
isWrappedHeap AType {}    = False

-- | userland-api C wrapper.
wrapperDecl
    :: String         -- ^ true C name
    -> String         -- ^ wrapper name
    -> WrappedType    -- ^ result type
    -> [WrappedType]  -- ^ arguments
    -> PC.Decl
wrapperDecl innerName wrapperName res args
    | isVoidW res
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName C.TypeVoid C.ImpureFunction (unwrapType <$> args')
          [PC.Expr $ PC.Call innerName (callArgs args' (PC.argsToIdx args'))]

    | isWrappedHeap res
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName C.TypeVoid C.ImpureFunction (unwrapType <$> (args' :> res))
          [PC.Assign (PC.LDeRef (PC.LVar IZ)) $ PC.Call innerName (callArgs args' (IS <$> PC.argsToIdx args'))]

    | otherwise
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName (unwrapType res) C.ImpureFunction (unwrapType <$> args')
          [PC.Return $ PC.Call innerName (callArgs args' (PC.argsToIdx args'))]
  where
    callArgs :: Env ctx' WrappedType -> Env ctx' (Idx ctx) -> [PC.Expr ctx]
    callArgs tys ids = toList (zipWithEnv f tys ids) where f ty idx = if isWrappedHeap ty then PC.DeRef (PC.Var idx) else PC.Var idx

hsWrapperDecl
    :: HsName NsVar   -- ^ haskell name
    -> HsName NsVar   -- ^ low-level import name
    -> WrappedType    -- ^ result type
    -> [WrappedType]  -- ^ arguments
    -> SHs.SDecl
hsWrapperDecl hiName loName res args = case res of
  HeapType {} ->
    SHs.DVar
      SHs.Var {
        varName    = hiName
      , varType    = SHs.translateType hsty
      , varExpr    = goA EmptyEnv args
      , varComment = Nothing
      }

  WrapType {} ->
    SHs.DVar
      SHs.Var {
        varName    = hiName
      , varType    = SHs.translateType hsty
      , varExpr    = goB EmptyEnv args
      , varComment = Nothing
      }

  CAType {} ->
    panicPure "ConstantArray cannot occur as a result type"

  AType {} ->
    panicPure "Array cannot occur as a result type"
  where
    hsty = foldr HsFun (HsIO $ typ' CFunRes $ unwrapOrigType res) (typ' CFunArg . unwrapOrigType <$> args)

    -- wrapper for fancy result
    goA :: Env ctx WrappedType -> [WrappedType] -> SHs.SExpr ctx
    goA env []     = goA' env (tabulateEnv (sizeEnv env) id) []
    goA env (x:xs) = SHs.ELam "x" $ goA (env :> x) xs

    goA' :: Env ctx' WrappedType -> Env ctx' (Idx ctx) -> [Idx ctx] -> SHs.SExpr ctx
    goA' EmptyEnv    EmptyEnv  zs
        = shsApps (SHs.EGlobal SHs.CAPI_allocaAndPeek)
          [ SHs.ELam "z" $ shsApps (SHs.EFree loName) (map SHs.EBound (fmap IS zs ++ [IZ]))
          ]

    goA' (tys :> ty) (xs :> x) zs = case ty of
        HeapType {} -> shsApps (SHs.EGlobal SHs.CAPI_with)
            [ SHs.EBound x
            , SHs.ELam "y" $ goA' tys (IS <$> xs) (IZ : fmap IS zs)
            ]

        CAType {} -> shsApps (SHs.EGlobal SHs.ConstantArray_withPtr)
            [ SHs.EBound x
            , SHs.ELam "ptr" $ goA' tys (IS <$> xs) (IZ : fmap IS zs)
            ]

        AType {} -> shsApps (SHs.EGlobal SHs.IncompleteArray_withPtr)
            [ SHs.EBound x
            , SHs.ELam "ptr" $ goA' tys (IS <$> xs) (IZ : fmap IS zs)
            ]

        WrapType {} ->
            goA' tys xs (x : zs)

    -- wrapper for non-fancy result.
    goB :: Env ctx WrappedType -> [WrappedType] -> SHs.SExpr ctx
    goB env []     = goB' env (tabulateEnv (sizeEnv env) id) []
    goB env (x:xs) = SHs.ELam "x" $ goB (env :> x) xs

    goB' :: Env ctx' WrappedType -> Env ctx' (Idx ctx) -> [Idx ctx] -> SHs.SExpr ctx
    goB' EmptyEnv    EmptyEnv  zs
        = shsApps (SHs.EFree loName) (map SHs.EBound zs)

    goB' (tys :> ty) (xs :> x) zs = case ty of
        HeapType {} -> shsApps (SHs.EGlobal SHs.CAPI_with)
          [ SHs.EBound x
          , SHs.ELam "y" $ goB' tys (IS <$> xs) (IZ : fmap IS zs)
          ]

        CAType {} -> shsApps (SHs.EGlobal SHs.ConstantArray_withPtr)
            [ SHs.EBound x
            , SHs.ELam "ptr" $ goB' tys (IS <$> xs) (IZ : fmap IS zs)
            ]

        AType {} -> shsApps (SHs.EGlobal SHs.IncompleteArray_withPtr)
            [ SHs.EBound x
            , SHs.ELam "ptr" $ goB' tys (IS <$> xs) (IZ : fmap IS zs)
            ]

        WrapType {} ->
            goB' tys xs (x : zs)

shsApps :: SHs.SExpr ctx -> [SHs.SExpr ctx] -> SHs.SExpr ctx
shsApps = foldl' SHs.EApp

functionDecs ::
     ModuleUnique
  -> Map C.Name C.Type -- ^ typedefs
  -> C.DeclInfo
  -> C.Function
  -> C.DeclSpec
  -> [Hs.Decl]
functionDecs mu typedefs info f _spec =
    [ Hs.DeclInlineCInclude $ getHashIncludeArg $ C.declHeader info
    , Hs.DeclInlineC $ PC.prettyDecl (wrapperDecl innerName wrapperName res args) ""
    , Hs.DeclForeignImport $ Hs.ForeignImportDecl
        { foreignImportName     = importName
        , foreignImportType     = importType
        , foreignImportOrigName = T.pack wrapperName
        , foreignImportCallConv = CallConvUserlandCAPI
        , foreignImportOrigin   = Origin.Function f
        , foreignImportComment  = fmap generateHaddocks
                                       (C.declComment info)
                               <> ioComment
        }
    ] ++
    [ Hs.DeclSimple $ hsWrapperDecl highlevelName importName res args
    | anyFancy
    ]
  where
    -- fancy types are heap types or constant arrays,
    -- i.e. ones we create high-level wrapper.
    anyFancy = any p (res : args) where
        p WrapType {} = False
        p HeapType {} = True
        p CAType {}   = True
        p AType {}    = True

    highlevelName = C.nameHs (C.declId info)
    importName
        | anyFancy   = highlevelName <> "_wrapper" -- TODO: Add to NameMangler pass
        | otherwise  = highlevelName

    res = wrapType $ C.functionRes f
    args = wrapType <$> C.functionArgs f
    attrs = C.functionAttrs f

    -- types which we cannot pass directly using C FFI.
    wrapType :: C.Type -> WrappedType
    wrapType oty = go oty
      where
        go C.TypeStruct {}         = HeapType oty
        go C.TypeUnion {}          = HeapType oty
        go (C.TypeConstArray n ty) = CAType oty n ty
        go (C.TypeIncompleteArray ty) = AType oty ty
        go (C.TypeTypedef ref)     = case ref of
          C.TypedefRegular n ->
            let t = Map.findWithDefault (panicPure $ "Unbound typedef " ++ show n) (C.nameC n) typedefs
            in go t
          C.TypedefSquashed _n ty' ->
            go ty'
        go _ = WrapType oty

    importType :: HsType
    importType = case res of
        HeapType {} ->
            foldr HsFun (HsIO $ typ' CFunRes C.TypeVoid) (typ' CFunArg . unwrapType <$> (args ++ [res]))

        WrapType {} ->
            foldr HsFun (hsIO $ typ' CFunRes $ unwrapType res) (typ' CFunArg . unwrapType <$> args)

        CAType {} ->
            panicPure "ConstantArray cannot occur as a result type"

        AType {} ->
            panicPure "Array cannot occur as a result type"

    -- | Decide based on the function attributes whether to include 'IO' in the
    -- result type of the foreign import. See the documentation on
    -- 'C.FunctionPurity'.
    --
    -- An exception to the rules: the foreign import function returns @void@
    -- when @res@ is a heap type, in which case a @const@ or @pure@ attribute
    -- does not make much sense, and so we just return the result in 'IO'.
    hsIO :: HsType -> HsType
    -- | C-pure functions can be safely encapsulated using 'unsafePerformIO' to
    -- create a Haskell-pure functions. We include a comment in the generated
    -- bindings to this effect.
    ioComment :: Maybe Hs.Comment
    (hsIO, ioComment) = case C.functionPurity attrs of
        C.HaskellPureFunction -> (id  , Nothing)
        C.CPureFunction       -> (HsIO, Just pureComment)
        C.ImpureFunction      -> (HsIO, Nothing)

    -- | A comment to put on bindings for C-pure functions
    --
    -- "Marked @__attribute((pure))__@"
    pureComment :: Hs.Comment
    pureComment =
      Hs.Comment {
        Hs.commentTitle    = Nothing
      , Hs.commentOrigin   = Nothing
      , Hs.commentChildren =
          [ Hs.Paragraph
            [ Hs.TextContent "Marked"
            , Hs.Monospace
              [ Hs.Bold
                [ Hs.TextContent "attribute((pure))" ]
              ]
            ]
          ]
      }

    -- below is generation of C wrapper for userland-capi.
    innerName :: String
    innerName = T.unpack (C.getName . C.nameC . C.declId $ info)

    wrapperName :: String
    wrapperName = unModuleUnique mu ++ "_" ++ innerName

{-------------------------------------------------------------------------------
  Globals
-------------------------------------------------------------------------------}

-- | Global variables
--
-- For by-reference foreign imports, @capi@ vs @ccall@ makes no difference:
-- @ghc@ does not create a wrapper. For non-extern non-static globals however it
-- is important that the header is imported /somewhere/, otherwise the global
-- variable is not linked in; we therefore add an explicit import. It is
-- important that we don't import such headers more than once, but this is taken
-- care of in 'csources'.
--
-- On Windows, simply generating a foreign import of a global variable's address
-- can lead to errors (see #898). For example, given a global @int
-- simpleGlobal@, the following foreign import might cause an error:
--
-- > foreign import capi safe "&simpleGlobal" simpleGlobal :: F.Ptr FC.CInt
--
-- So, instead we generate a /stub/ function that simply returns the address of
-- the global variable ...
--
-- > __attribute__ ((const)) signed int *get_simpleGlobal_ptr (void) {
-- >   return &simpleGlobal;
-- > }
--
-- ... and then create a foreign import for the stub.
--
-- > foreign import capi safe "get_simpleGlobal_ptr" simpleGlobal :: F.Ptr FC.CInt
--
-- Note that stub function also has a @const@ function attribute to emphasise
-- that the function always returns the same address throughout the lifetime of
-- the program, which means we can omit the 'IO' from the foreign import.
--
global :: C.DeclInfo -> C.Type -> C.DeclSpec -> [Hs.Decl]
global info ty _spec =
    [ Hs.DeclInlineCInclude (getHashIncludeArg $ C.declHeader info)
    , Hs.DeclInlineC prettyStub
    , Hs.DeclForeignImport $ Hs.ForeignImportDecl
        { foreignImportName     = importName
        , foreignImportType     = importType
        , foreignImportOrigName = T.pack stubName
        , foreignImportCallConv = CallConvUserlandCAPI
        , foreignImportOrigin   = Origin.Global ty
        , foreignImportComment  = fmap generateHaddocks (C.declComment info)
        }
    ]
  where
    importName :: HsName 'NsVar
    importName = C.nameHs (C.declId info)

    importType :: HsType
    importType = typ stubType

    -- TODO: the stub name should go through the name mangler. See #946.
    stubName :: String
    stubName = "get_" ++ varName ++ "_ptr"

    varName :: String
    varName = T.unpack (C.getName . C.nameC . C.declId $ info)

    stubType :: C.Type
    stubType = C.TypePointer ty

    prettyStub :: String
    prettyStub = PC.prettyDecl stubDecl " "

    stubDecl :: PC.Decl
    stubDecl =
        PC.withArgs [] $ \args' ->
          PC.FunDefn stubName stubType C.HaskellPureFunction args'
            [PC.Return $ PC.Address $ PC.NamedVar varName]

globalConst :: C.DeclInfo -> C.Type -> C.DeclSpec -> [Hs.Decl]
globalConst = throwPure_TODO 41 "Constants not yet supported"

{-------------------------------------------------------------------------------
  Macro
-------------------------------------------------------------------------------}

macroVarDecs ::
     C.DeclInfo
  -> C.CheckedMacroExpr
  -> [Hs.Decl]
macroVarDecs info macroExpr = [
      Hs.DeclVar $
        Hs.VarDecl
          { varDeclName    = hsVarName
          , varDeclType    = quantTyHsTy macroExprType
          , varDeclBody    = hsBody
          , varDeclComment = fmap generateHaddocks (C.declComment info)
          }
    | hsBody <- toList $ macroLamHsExpr macroExprArgs macroExprBody
    ]
  where
    macroExprArgs :: [C.Name]
    macroExprBody :: C.MExpr C.Ps
    macroExprType :: Macro.Quant (Macro.Type Macro.Ty)
    C.CheckedMacroExpr{macroExprArgs, macroExprBody, macroExprType} = macroExpr

    hsVarName :: HsName NsVar
    hsVarName = C.nameHs (C.declId info)

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
    goTy env (Macro.TyVarTy tv) =
      case Map.lookup (Macro.tyVarName tv) env of
        Just hsTv ->
          Hs.TyVarTy hsTv
        Nothing ->
          panicPure $ unlines
            [ "quantTyHsTy: unbound type variable " ++ show tv
            , "env: " ++ show env
            , "macro: " ++ show (Macro.mkQuantTyBody qty)
            ]
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
     [C.Name]
  -> C.MExpr p
  -> Maybe (Hs.VarDeclRHS EmptyCtx)
macroLamHsExpr macroArgs expr =
    makeNames macroArgs Map.empty
  where
    makeNames :: [C.Name] -> Map C.Name (Idx ctx) -> Maybe (Hs.VarDeclRHS ctx)
    makeNames []     env = macroExprHsExpr env expr
    makeNames (n:ns) env = Hs.VarDeclLambda . Hs.Lambda (cnameToHint n) <$> makeNames ns (Map.insert n IZ (fmap IS env))

cnameToHint :: C.Name -> NameHint
cnameToHint (C.Name t) = fromString (T.unpack t)

macroExprHsExpr ::
     Map C.Name (Idx ctx)
  -> C.MExpr p
  -> Maybe (Hs.VarDeclRHS ctx)
macroExprHsExpr = goExpr where
    goExpr :: Map C.Name (Idx ctx) -> C.MExpr p -> Maybe (Hs.VarDeclRHS ctx)
    goExpr env = \case
      C.MTerm tm -> goTerm env tm
      C.MApp _xapp fun args ->
        goApp env (Hs.InfixAppHead fun) (toList args)

    goTerm :: Map C.Name (Idx ctx) -> C.MTerm p -> Maybe (Hs.VarDeclRHS ctx)
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
            let hsVar = macroName cname -- mangle nm $ NameVar cname
            in  goApp env (Hs.VarAppHead hsVar) args
      C.MStringize {} -> Nothing
      C.MConcat {} -> Nothing

    goApp :: Map C.Name (Idx ctx) -> Hs.VarDeclRHSAppHead -> [C.MExpr p] -> Maybe (Hs.VarDeclRHS ctx)
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

-- | Construct Haskell name for macro
--
-- TODO: This should be done as part of the NameMangler frontend pass.
macroName :: C.Name -> HsName NsVar
macroName (C.Name cName) =
    case FixCandidate.fixCandidate fix cName of
      Just hsName -> hsName
      Nothing     ->
        panicPure $ "Unable to construct name for macro " ++ show cName
  where
    fix :: FixCandidate Maybe
    fix = FixCandidate.fixCandidateDefault
