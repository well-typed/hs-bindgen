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
import Data.Type.Nat (SNatI, induction)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vec.Lazy qualified as Vec
import GHC.Exts qualified as IsList (IsList(..))

import C.Char qualified as C

import Clang.Paths
import HsBindgen.C.AST qualified as C
import HsBindgen.C.Tc.Macro qualified as Macro
import HsBindgen.Errors
import HsBindgen.ExtBindings (HsTypeClass)
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type
import HsBindgen.Hs.NameMangler
import HsBindgen.Imports
import HsBindgen.NameHint
import HsBindgen.ModuleUnique

import DeBruijn
  (Idx (..), pattern I1, weaken, Add (..), pattern I2, EmptyCtx)

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

data TranslationOpts = TranslationOpts {
      -- | Default set of classes to derive for structs
      translationDeriveStruct :: [(Hs.Strategy Hs.HsType, HsTypeClass)]

      -- | Default set of classes to derive for enums
    , translationDeriveEnum :: [(Hs.Strategy Hs.HsType, HsTypeClass)]

      -- | Default set of classes to derive for typedefs around primitive types
      --
      -- The situation for typedefs is more intricate, because the instances we
      -- can generate depend on the instances available for the type we're
      -- defining a newtype for. However, for typedefs of /primitive/ types
      -- this is easier, as we do know which instances are available for those.
      --
      -- Any classes in this list that are /not/ supported by the underlying
      -- (primitive) type will simply not be generated, so it's okay for this
      -- to contain classes such as 'Num' which are only supported by /some/
      -- primitive types.
    , translationDeriveTypedefPrim :: [(Hs.Strategy Hs.HsType, HsTypeClass)]
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
        , (Hs.DeriveStock, Hs.Read)
        ]
    , translationDeriveTypedefPrim = [
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
generateDeclarations opts _mu nm (C.Header decs) =
    flip State.evalState Map.empty $
      concat <$> mapM (generateDecs opts nm typedefs) decs
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

{-------------------------------------------------------------------------------
  Declarations
------------------------------------------------------------------------------}

generateDecs ::
     State.MonadState InstanceMap m
  => TranslationOpts
  -> NameMangler
  -> Map C.CName C.Type
  -> C.Decl
  -> m [Hs.Decl]
generateDecs opts nm typedefs = \case
    C.DeclStruct struct  -> reifyStructFields struct $ structDecs opts nm struct
    C.DeclUnion union    -> unionDecs opts nm union
    C.DeclOpaqueStruct o -> opaqueStructDecs opts nm o
    C.DeclEnum e         -> enumDecs opts nm e
    C.DeclOpaqueEnum o   -> opaqueEnumDecs opts nm o -- TODO?
    C.DeclTypedef d      -> typedefDecs opts nm d
    C.DeclMacro m        -> macroDecs opts nm m
    C.DeclFunction f     -> functionDecs opts nm typedefs f

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
structDecs opts nm struct fields = return $ concat
    [ [ Hs.DeclData hs ]
    , [ Hs.DeclDefineInstance $ Hs.InstanceStorable hs storable]
    , [ Hs.DeclDeriveInstance strat clss (Hs.structName hs)
      | (strat, clss) <- translationDeriveStruct opts
      ]
    , flamInstance
    ]
  where
    hs :: Hs.Struct n
    hs =
      let structName = mangle nm $ NameTycon $ C.structDeclPath struct
          structConstr = mangle nm $ NameDatacon $ C.structDeclPath struct
          structFields = flip Vec.map fields $ \f -> Hs.Field {
              fieldName   = mangle nm $ NameField (C.structDeclPath struct) (C.fieldName f)
            , fieldType   = typ nm (C.fieldType f)
            , fieldOrigin = Hs.FieldOriginStructField f
            }
          structOrigin = Hs.StructOriginStruct struct
          structInstances = Set.empty
      in  Hs.Struct{..}

    storable :: Hs.StorableInstance
    storable = Hs.StorableInstance {
          Hs.storableSizeOf    = C.structSizeof struct
        , Hs.storableAlignment = C.structAlignment struct
        , Hs.storablePeek      = Hs.Lambda "ptr" $
            Hs.Ap (Hs.StructCon hs) $ map (peek IZ) (C.structFields struct)
        , Hs.storablePoke      = Hs.Lambda "ptr" $ Hs.Lambda "s" $
            Hs.makeElimStruct IZ hs $ \wk xs -> Hs.Seq $ toList $ Vec.zipWith (poke (weaken wk I1)) fields xs
        }

    peek :: Idx ctx -> C.StructField -> Hs.PeekByteOff ctx
    peek ptr f = case C.fieldWidth f of
      Nothing -> Hs.PeekByteOff ptr (C.fieldOffset f `div` 8)
      Just w  -> Hs.PeekBitOffWidth ptr (C.fieldOffset f) w

    poke :: Idx ctx -> C.StructField -> Idx ctx -> Hs.PokeByteOff ctx
    poke ptr f i = case C.fieldWidth f of
      Nothing -> Hs.PokeByteOff ptr (C.fieldOffset f `div` 8) i
      Just w  -> Hs.PokeBitOffWidth ptr (C.fieldOffset f) w i

    flamInstance :: [Hs.Decl]
    flamInstance = case C.structFlam struct of
      Nothing  -> []
      Just flam -> singleton $ Hs.DeclDefineInstance $ Hs.InstanceHasFLAM
        hs
        (typ nm (C.fieldType flam))
        (C.fieldOffset flam `div` 8)

{-------------------------------------------------------------------------------
  Opaque struct and opaque enum
-------------------------------------------------------------------------------}

opaqueStructDecs ::
     State.MonadState InstanceMap m
  => TranslationOpts
  -> NameMangler
  -> C.OpaqueStruct
  -> m [Hs.Decl]
opaqueStructDecs _opts nm o = return $
    [ Hs.DeclEmpty Hs.EmptyData {
          emptyDataName   = mangle nm $ NameTycon $ C.DeclPathName (C.opaqueStructTag o)
        , emptyDataOrigin = Hs.EmptyDataOriginOpaqueStruct o
        }
    ]

opaqueEnumDecs ::
     State.MonadState InstanceMap m
  => TranslationOpts
  -> NameMangler
  -> C.OpaqueEnum
  -> m [Hs.Decl]
opaqueEnumDecs _opts nm o = return $
    [ Hs.DeclEmpty Hs.EmptyData {
          emptyDataName   = mangle nm $ NameTycon $ C.DeclPathName (C.opaqueEnumTag o)
        , emptyDataOrigin = Hs.EmptyDataOriginOpaqueEnum o
        }
    ]

{-------------------------------------------------------------------------------
  Unions
-------------------------------------------------------------------------------}

unionDecs ::
     State.MonadState InstanceMap m
  => TranslationOpts
  -> NameMangler
  -> C.Union
  -> m [Hs.Decl]
unionDecs _opts nm union = return $
    [ Hs.DeclNewtype Hs.Newtype {..}
    , Hs.DeclDeriveInstance (Hs.DeriveVia sba) Hs.Storable newtypeName
    ] ++ concat
    [ [ Hs.DeclUnionGetter newtypeName (typ nm ufieldType) (mangle nm $ NameGetter declPath ufieldName)
      , Hs.DeclUnionSetter newtypeName (typ nm ufieldType) (mangle nm $ NameBuilder declPath ufieldName)
      ]
    | C.UnionField {..} <- C.unionFields union
    ]
  where
    declPath      = C.unionDeclPath union
    newtypeName   = mangle nm $ NameTycon declPath
    newtypeConstr = mangle nm $ NameDatacon declPath
    newtypeField  = Hs.Field {
        fieldName   = mangle nm $ NameDecon declPath
      , fieldType   = Hs.HsByteArray
      , fieldOrigin = Hs.FieldOriginNone
      }
    newtypeOrigin = Hs.NewtypeOriginUnion union
    newtypeInstances = Set.empty

    sba :: Hs.HsType
    sba = HsSizedByteArray (fromIntegral (C.unionSizeof union)) (fromIntegral (C.unionAlignment union))

{-------------------------------------------------------------------------------
  Enum
-------------------------------------------------------------------------------}

enumDecs ::
     State.MonadState InstanceMap m
  => TranslationOpts
  -> NameMangler
  -> C.Enu
  -> m [Hs.Decl]
enumDecs opts nm e = return $ concat [
      [ Hs.DeclNewtype Hs.Newtype{..} ]
    , [ Hs.DeclDefineInstance $ Hs.InstanceStorable hs storable ]
    , [ Hs.DeclDeriveInstance strat clss (Hs.structName hs)
      | (strat, clss) <- translationDeriveEnum opts
      ]
    , cEnumInstanceDecls
    , valueDecls
    ]
  where
    declPath      = C.enumDeclPath e
    newtypeName   = mangle nm $ NameTycon declPath
    newtypeConstr = mangle nm $ NameDatacon declPath
    newtypeField  = Hs.Field {
        fieldName   = mangle nm $ NameDecon declPath
      , fieldType   = typ nm (C.enumType e)
      , fieldOrigin = Hs.FieldOriginNone
      }
    newtypeOrigin = Hs.NewtypeOriginEnum e
    newtypeInstances = Set.empty

    hs :: Hs.Struct (S Z)
    hs =
      let structName = newtypeName
          structConstr = newtypeConstr
          structFields = Vec.singleton newtypeField
          structOrigin = Hs.StructOriginEnum e
          structInstances = Set.empty
      in  Hs.Struct{..}

    storable :: Hs.StorableInstance
    storable = Hs.StorableInstance {
          Hs.storableSizeOf    = C.enumSizeof e
        , Hs.storableAlignment = C.enumAlignment e
        , Hs.storablePeek      = Hs.Lambda "ptr" $
            Hs.Ap (Hs.StructCon hs) [ peek IZ 0 ]
        , Hs.storablePoke      = Hs.Lambda "ptr" $ Hs.Lambda "s" $
            Hs.ElimStruct IZ hs (AS AZ) $ Hs.Seq [ poke I2 0 IZ ]
        }

    peek :: Idx ctx -> Int -> Hs.PeekByteOff ctx
    peek = Hs.PeekByteOff

    poke :: Idx ctx -> Int -> Idx ctx -> Hs.PokeByteOff ctx
    poke = Hs.PokeByteOff

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
            Hs.InstanceCEnum hs fTyp vStrs (isJust mSeqBounds)
          cEnumShowDecl = [Hs.DeclDefineInstance (Hs.InstanceCEnumShow hs)]
          sequentialCEnumDecl = case mSeqBounds of
            Just (nameMin, nameMax) -> List.singleton . Hs.DeclDefineInstance $
              Hs.InstanceSequentialCEnum hs nameMin nameMax
            Nothing -> []
      in  cEnumDecl : sequentialCEnumDecl ++ cEnumShowDecl

{-------------------------------------------------------------------------------
  Typedef
-------------------------------------------------------------------------------}

typedefDecs ::
     State.MonadState InstanceMap m
  => TranslationOpts
  -> NameMangler
  -> C.Typedef
  -> m [Hs.Decl]
typedefDecs opts nm d = return $ concat [
      [ Hs.DeclNewtype Hs.Newtype{..} ]
    , [ Hs.DeclDeriveInstance Hs.DeriveNewtype Hs.Storable newtypeName ]
    , [ Hs.DeclDeriveInstance strat clss newtypeName
      | C.TypePrim pt <- [C.typedefType d]
      , (strat, clss) <- translationDeriveTypedefPrim opts
      , clss `elem` primTypeInstances pt
      ]
    ]
  where
    cName         = C.typedefName d
    newtypeName   = mangle nm $ NameTycon (C.DeclPathName cName)
    newtypeConstr = mangle nm $ NameDatacon (C.DeclPathName cName)
    newtypeField  = Hs.Field {
        fieldName   = mangle nm $ NameDecon (C.DeclPathName cName)
      , fieldType   = typ nm (C.typedefType d)
      , fieldOrigin = Hs.FieldOriginNone
      }
    newtypeOrigin = Hs.NewtypeOriginTypedef d
    newtypeInstances = Set.empty

primTypeInstances :: C.PrimType -> [HsTypeClass]
primTypeInstances (C.PrimFloating _) = [
      Hs.Enum
    , Hs.Floating
    , Hs.RealFloat
    , Hs.Storable
    , Hs.Num
    , Hs.Read
    , Hs.Fractional
    , Hs.Real
    , Hs.RealFrac
    , Hs.Show
    , Hs.Eq
    , Hs.Ord
    ]
primTypeInstances _otherwise = [
      Hs.Bits
    , Hs.FiniteBits
    , Hs.Bounded
    , Hs.Enum
    , Hs.Storable
    , Hs.Ix
    , Hs.Num
    , Hs.Read
    , Hs.Integral
    , Hs.Real
    , Hs.Show
    , Hs.Eq
    , Hs.Ord
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
    = return $ macroDecsTypedef opts nm m

    | otherwise
    = return $ macroVarDecs nm m ty
    where

macroDecs _ _ C.MacroReparseError {} = return []
macroDecs _ _ C.MacroTcError {}      = return []

macroDecsTypedef :: TranslationOpts -> NameMangler -> C.Macro -> [Hs.Decl]
macroDecsTypedef opts nm m =
    case C.macroBody m of
      C.TypeMacro tyNm
        | Right ty <- C.typeNameType tyNm
        ->
        let newtypeField = mkField ty in
        concat [
            [ Hs.DeclNewtype Hs.Newtype{..} ]
          , [ Hs.DeclDeriveInstance Hs.DeriveNewtype Hs.Storable newtypeName ]
          , [ Hs.DeclDeriveInstance strat clss newtypeName
            | C.TypePrim pt <- [ty]
            , (strat, clss) <- translationDeriveTypedefPrim opts
            , clss `elem` primTypeInstances pt
            ]
          ]
      _otherwise ->
        []
  where
    cName         = C.macroName m
    newtypeName   = mangle nm $ NameTycon (C.DeclPathName cName)
    newtypeConstr = mangle nm $ NameDatacon (C.DeclPathName cName)
    newtypeOrigin = Hs.NewtypeOriginMacro m
    newtypeInstances = Set.empty

    mkField :: C.Type -> Hs.Field
    mkField ty = Hs.Field {
          fieldName   = mangle nm $ NameDecon (C.DeclPathName cName)
        , fieldType   = typ nm ty
        , fieldOrigin = Hs.FieldOriginNone
        }

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
    go _ (C.TypeConstArray n ty) =
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

functionDecs ::
     State.MonadState InstanceMap m
  => TranslationOpts
  -> NameMangler
  -> Map C.CName C.Type -- ^ typedefs
  -> C.Function
  -> m [Hs.Decl]
functionDecs _opts nm typedefs f
  | any isFancy (C.functionRes f : C.functionArgs f)
  = throwPure_TODO 37 "Struct value arguments and results are not supported"
  | otherwise = return $
    [ Hs.DeclForeignImport $ Hs.ForeignImportDecl
        { foreignImportName       = mangle nm $ NameVar $ C.functionName f
        , foreignImportType       = ty
        , foreignImportCRes       = C.functionRes f
        , foreignImportCArgs      = C.functionArgs f
        , foreignImportOrigName   = C.getCName $ C.functionName f
        , foreignImportHeader     = getCHeaderIncludePath $ C.functionHeader f
        , foreignImportDeclOrigin = Hs.ForeignImportDeclOriginFunction f
        }
    ]
  where
    -- types which we cannot pass directly using C FFI.
    isFancy :: C.Type -> Bool
    isFancy C.TypeStruct {}     = True
    isFancy C.TypeUnion {}      = True
    isFancy C.TypeConstArray {} = True
    isFancy (C.TypeTypedef n)   =
        let t = Map.findWithDefault (panicPure $ "Unbound typedef " ++ show n) n typedefs
        in isFancy t
    isFancy _ = False

    ty :: HsType
    ty = foldr HsFun (HsIO $ typ' CFunRes nm $ C.functionRes f) (typ' CFunArg nm <$> C.functionArgs f)

{-------------------------------------------------------------------------------
  Macro
-------------------------------------------------------------------------------}

macroVarDecs ::
     NameMangler
  -> C.Macro
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
  -> C.MacroBody
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
  -> C.MExpr
  -> Maybe (Hs.VarDeclRHS ctx)
macroExprHsExpr nm = goExpr where
    goExpr :: Map C.CName (Idx ctx) -> C.MExpr -> Maybe (Hs.VarDeclRHS ctx)
    goExpr env = \case
      C.MTerm tm -> goTerm env tm
      C.MApp fun args ->
        goApp env (Hs.InfixAppHead fun) (toList args)

    goTerm :: Map C.CName (Idx ctx) -> C.MTerm -> Maybe (Hs.VarDeclRHS ctx)
    goTerm env = \case
      C.MInt i -> goInt i
      C.MFloat f -> goFloat f
      C.MChar c -> goChar c
      C.MString s -> goString s
      C.MVar cname args ->
        --  TODO: removed the macro argument used as a function check.
        case Map.lookup cname env of
          Just i  -> return (Hs.VarDeclVar i)
          Nothing ->
            let hsVar = mangle nm $ NameVar cname
            in  goApp env (Hs.VarAppHead hsVar) args
      C.MStringize {} -> Nothing
      C.MConcat {} -> Nothing

    goApp :: Map C.CName (Idx ctx) -> Hs.VarDeclRHSAppHead -> [C.MExpr] -> Maybe (Hs.VarDeclRHS ctx)
    goApp env appHead args = do
      args' <- traverse (goExpr env) args
      return $ Hs.VarDeclApp appHead args'

    goInt :: C.IntegerLiteral -> Maybe (Hs.VarDeclRHS ctx)
    goInt (C.IntegerLiteral { integerLiteralType = mbIntTy, integerLiteralValue = i }) =
      Just $ Hs.VarDeclIntegral i (maybe HsPrimCInt (uncurry integralType) mbIntTy)

    goChar :: C.CharLiteral -> Maybe (Hs.VarDeclRHS ctx)
    goChar (C.CharLiteral { charLiteralValue = c }) =
      return $ Hs.VarDeclChar c

    goString :: C.StringLiteral -> Maybe (Hs.VarDeclRHS ctx)
    goString (C.StringLiteral { stringLiteralValue = s }) = do
      let bytes = concatMap (IsList.toList . C.charValue) s
      return $
        Hs.VarDeclString (IsList.fromList bytes)

    goFloat :: C.FloatingLiteral -> Maybe (Hs.VarDeclRHS ctx)
    goFloat flt@(C.FloatingLiteral { floatingLiteralType = mbFty }) =
      case mbFty of
        Nothing -> Just $ Hs.VarDeclDouble (C.floatingLiteralDoubleValue flt)
        Just fty ->
          case fty of
            C.PrimFloat  -> Just $ Hs.VarDeclFloat (C.floatingLiteralFloatValue flt)
            C.PrimDouble -> Just $ Hs.VarDeclDouble (C.floatingLiteralDoubleValue flt)
            C.PrimLongDouble -> Nothing
