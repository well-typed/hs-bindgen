{-# LANGUAGE NamedFieldPuns #-}

module HsBindgen.Backend.Hs.Translation.Structure (
    structDecs
  ) where

import Control.Monad.State qualified as State hiding (MonadState)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Type.Nat (SNatI)
import Data.Vec.Lazy qualified as Vec
import DeBruijn (Idx (..), Weaken (..), pattern I1)

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.Haddock.Config (HaddockConfig)
import HsBindgen.Backend.Hs.Haddock.Translation
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Instances qualified as Hs
import HsBindgen.Backend.Hs.Translation.Prim qualified as HsPrim
import HsBindgen.Backend.Hs.Translation.State (HsM)
import HsBindgen.Backend.Hs.Translation.State qualified as State
import HsBindgen.Backend.Hs.Translation.Type qualified as Type
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.MangleNames.IsPass qualified as MangleNames
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs

-- | Generate declarations for given C struct
structDecs ::
     Map Inst.TypeClass Inst.SupportedStrategies
  -> HaddockConfig
  -> C.DeclInfo Final
  -> C.Struct Final
  -> PrescriptiveDeclSpec
  -> HsM [Hs.Decl]
structDecs supInsts hCfg info struct spec =
    reifyStructFields $ case struct.flam of
      Nothing   -> getDeclsFieldVec          supInsts hCfg spec info struct
      Just flam -> getDeclsFieldVecFlam flam supInsts hCfg spec info struct
  where
    reifyStructFields ::
      (forall n. SNatI n => Vec n (C.StructField Final) -> a) -> a
    reifyStructFields k = Vec.reifyList struct.fields k

getDeclsFieldVec :: forall n.
     SNatI n
  => Map Inst.TypeClass Inst.SupportedStrategies
  -> HaddockConfig
  -> PrescriptiveDeclSpec
  -> C.DeclInfo Final
  -> C.Struct Final
  -> Vec n (C.StructField Final)
  -> HsM [Hs.Decl]
getDeclsFieldVec supInsts hCfg spec info struct fieldsVec = do
    insts <-
      getInstances supInsts name struct.fields <$> State.gets (.instanceMap)
    let (hsStruct, decls) =
          getDecls supInsts hCfg spec name info struct fieldsVec insts
    State.modify' $ #instanceMap %~ Map.insert name hsStruct.instances
    pure $ Hs.DeclData hsStruct : decls
  where
    name :: Hs.Name Hs.NsTypeConstr
    name = Hs.unsafeHsIdHsName info.id.unsafeHsName

getDeclsFieldVecFlam :: forall n.
     SNatI n
  => C.StructField Final
  -> Map Inst.TypeClass Inst.SupportedStrategies
  -> HaddockConfig
  -> PrescriptiveDeclSpec
  -> C.DeclInfo Final
  -> C.Struct Final
  -> Vec n (C.StructField Final)
  -> HsM [Hs.Decl]
getDeclsFieldVecFlam flam supInsts hCfg spec info struct fieldsVec = do
    insts <-
      getInstances supInsts auxName struct.fields <$> State.gets (.instanceMap)
    let insts' = Set.insert Inst.Flam_Offset insts
        (hsStruct, decls) =
          getDecls supInsts hCfg spec auxName info struct fieldsVec insts'
    State.modify' $ #instanceMap %~ Map.insert auxName hsStruct.instances
    pure $ Hs.DeclData hsStruct : decls ++ [getHasFlamInstanceDecl hsStruct, flamDecl]
  where
    name :: Hs.Name Hs.NsTypeConstr
    name = Hs.unsafeHsIdHsName info.id.unsafeHsName

    auxName :: Hs.Name Hs.NsTypeConstr
    auxName = Hs.unsafeHsIdHsName $ info.id.unsafeHsName <> "_Aux"

    getHasFlamInstanceDecl :: Hs.Struct n -> Hs.Decl
    getHasFlamInstanceDecl hsStruct =
      Hs.DeclDefineInstance
        Hs.DefineInstance{
            comment      = Nothing
          , instanceDecl =
              Hs.InstanceHasFlam
                hsStruct
                (Type.topLevel flam.typ)
                (flam.offset `div` 8)
          }

    -- TODO: generate zero-copy bindings for the FLAM field. See issue
    -- #1286.
    flamDecl :: Hs.Decl
    flamDecl =
      Hs.DeclTypSyn
        Hs.TypSyn{
            name
          , typ     =
              Hs.HsWithFlam
                (Type.topLevel flam.typ)
                (Hs.HsTypRef auxName Nothing)
          , origin  = Origin.Decl{
              info = info
            , kind = Origin.Opaque info.id.cName.name.kind
            , spec = spec
            }
          , comment = mkHaddocks hCfg info name
          }

getInstances ::
     Map Inst.TypeClass Inst.SupportedStrategies
  -> Hs.Name Hs.NsTypeConstr
  -> [C.StructField Final]
  -> Hs.InstanceMap
  -> Set Inst.TypeClass
getInstances supInsts structName fields instanceMap =
    Hs.getInstances instanceMap (Just structName) candidateInsts fieldTypes
  where
    fieldTypes :: [Hs.HsType]
    fieldTypes = Type.topLevel . (.typ) <$> fields

    candidateInsts :: Set Inst.TypeClass
    candidateInsts = Hs.getCandidateInsts supInsts

getDecls :: forall n.
     SNatI n
  => Map Inst.TypeClass Inst.SupportedStrategies
  -> HaddockConfig
  -> PrescriptiveDeclSpec
  -> Hs.Name Hs.NsTypeConstr
  -> C.DeclInfo Final
  -> C.Struct Final
     -- TODO https://github.com/well-typed/hs-bindgen/issues/1576: The field
     -- vector contains information about the number of fields on the type
     -- level. Tracking this information here may not be necessary.
  -> Vec n (C.StructField Final)
  -> Set Inst.TypeClass
  -> (Hs.Struct n, [Hs.Decl])
getDecls supInsts hCfg spec structName info struct fieldsVec insts =
    ( hsStruct
    , storableDecl ++ primDecl ++ optDecls ++ fieldDecls
    )
  where
    getHsField :: C.StructField Final -> Hs.Field
    getHsField field =
        Hs.Field {
            name    = Hs.unsafeHsIdHsName field.info.name.hsName
          , typ     = Type.topLevel field.typ
          , origin  = Origin.StructField field
          , comment = mkHaddocksFieldInfo hCfg info field.info
          }

    hsStruct :: Hs.Struct n
    hsStruct = Hs.Struct {
          name      = structName
        , constr    = struct.names.constr
        , fields    = Vec.map getHsField fieldsVec
        , instances = insts <> knownInsts
        , comment   = mkHaddocks hCfg info structName
        , origin    = Just Origin.Decl{
              info
            , kind = Origin.Struct struct
            , spec
            }
        }

    storableDecl :: [Hs.Decl]
    storableDecl
      | Inst.Storable `Set.notMember` insts = []
      | otherwise = singleton $ Hs.DeclDefineInstance
          Hs.DefineInstance {
            comment      = Nothing
          , instanceDecl = Hs.InstanceStorable hsStruct Hs.StorableInstance{
                sizeOf    = struct.sizeof
              , alignment = struct.alignment
              , peek      = Hs.Lambda "ptr" $
                  Hs.Ap (Hs.StructCon hsStruct) $
                    map (peekField IZ) struct.fields
              , poke      = Hs.Lambda "ptr" $ Hs.Lambda "s" $
                  Hs.makeElimStruct IZ hsStruct $ \wk xs -> Hs.Seq $ Vec.toList $
                    Vec.zipWith (pokeField (weaken wk I1))
                      fieldsVec xs
              }
          }

    primDecl :: [Hs.Decl]
    primDecl = HsPrim.mkPrimInstance insts hsStruct struct

    optDecls :: [Hs.Decl]
    optDecls = catMaybes [
        case Hs.getDeriveStrat supStrats of
          Nothing    -> Nothing
          Just strat -> Just $ Hs.DeclDeriveInstance Hs.DeriveInstance{
              name     = structName
            , clss     = clss
            , strategy = strat
            , comment  = Nothing
            }
      | (clss, supStrats) <- Map.assocs supInsts
      , clss `Set.member` insts
      ]

    fieldDecls :: [Hs.Decl]
    fieldDecls = concatMap (getFieldDecls structName) struct.fields

    knownInsts :: Set Inst.TypeClass
    knownInsts = Set.fromList $ catMaybes [
        if any (isJust . (.width)) struct.fields
          then Just Inst.HasCBitField
          else Nothing
      , if any (isNothing . (.width)) struct.fields
          then Just Inst.HasCField
          else Nothing
      , if null struct.fields then Nothing else Just Inst.HasField
      ]

{-------------------------------------------------------------------------------
  Fields
-------------------------------------------------------------------------------}

-- | 'HasCField', 'HasCBitfield', and 'HasField' instances for a field of a
-- struct declaration
--
-- Given a struct:
--
-- > struct myStruct { int x; char y };
--
-- We generate roughly this datatype:
--
-- > newtype MyStruct = MyStruct { myStruct_x :: CInt, myStruct_y :: CChar }
--
-- Then, 'structFieldDecls' will generate roughly the following class instances
-- for the fields @x@ and @y@ respectively:
--
-- > instance HasCField "myStruct_x" MyStruct where
-- >   type CFieldType "myStruct_x" MyStruct = CInt
-- > instance HasField "myStruct_x" (Ptr MyStruct) (Ptr CInt)
--
-- > instance HasCField "myStruct_y" MyStruct where
-- >   type CFieldType "myStruct_y" MyStruct = CChar
-- > instance HasField "myStruct_y" (Ptr MyStruct) (Ptr CChar)
--
-- This works similarly for bit-fields, but those get a 'HasCBitfield' instance
-- instead of a 'HasCField' instance.
getFieldDecls :: Hs.Name Hs.NsTypeConstr -> C.StructField Final -> [Hs.Decl]
getFieldDecls structName field = [
      Hs.DeclDefineInstance $
        Hs.DefineInstance {
            comment      = Nothing
          , instanceDecl =
              case field.width of
                Nothing -> Hs.InstanceHasCField $ hasCFieldDecl
                Just w  -> Hs.InstanceHasCBitfield $ hasCBitfieldDecl w
          }
    , Hs.DeclDefineInstance $
        Hs.DefineInstance {
            comment      = Nothing
          , instanceDecl = Hs.InstanceHasField hasFieldDecl
          }
    ]
  where
    parentType :: HsType
    parentType = Hs.HsTypRef structName Nothing

    fieldName :: Hs.Name Hs.NsVar
    fieldName = Hs.unsafeHsIdHsName field.info.name.hsName

    fieldType :: HsType
    fieldType = Type.topLevel field.typ

    hasFieldDecl :: Hs.HasFieldInstance
    hasFieldDecl = Hs.HasFieldInstance {
          parentType = parentType
        , fieldName  = fieldName
        , fieldType  = fieldType
        , deriveVia  =
            case field.width of
              Nothing -> Hs.ViaHasCField
              Just _  -> Hs.ViaHasCBitfield
        }

    hasCFieldDecl :: Hs.HasCFieldInstance
    hasCFieldDecl = Hs.HasCFieldInstance {
          parentType  = parentType
        , fieldName   = fieldName
        , cFieldType  = fieldType
        , fieldOffset = field.offset `div` 8
        }

    hasCBitfieldDecl :: Int -> Hs.HasCBitfieldInstance
    hasCBitfieldDecl w = Hs.HasCBitfieldInstance {
          parentType    = parentType
        , fieldName     = fieldName
        , cBitfieldType = fieldType
        , bitOffset     = field.offset
        , bitWidth      = w
        }

peekField :: Idx ctx -> C.StructField Final -> Hs.PeekCField ctx
peekField ptr field = case field.width of
    Nothing -> Hs.PeekCField    (HsStrLit name) ptr
    Just _w -> Hs.PeekCBitfield (HsStrLit name) ptr
  where
    name = Text.unpack field.info.name.hsName.text

pokeField :: Idx ctx -> C.StructField Final -> Idx ctx -> Hs.PokeCField ctx
pokeField ptr field x = case field.width of
    Nothing  -> Hs.PokeCField    (HsStrLit name) ptr x
    Just _w  -> Hs.PokeCBitfield (HsStrLit name) ptr x
  where
    name = Text.unpack field.info.name.hsName.text
