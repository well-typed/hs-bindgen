{-# LANGUAGE NamedFieldPuns #-}

module HsBindgen.Backend.Hs.Translation.Structure (
    structDecs
  ) where

import Control.Monad.Reader qualified as Reader
import Control.Monad.State qualified as State hiding (MonadState)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vec.Lazy qualified as Vec
import DeBruijn (EmptyCtx, Idx (..), Weaken (..), pattern I1)

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Haddock.Config (HaddockConfig)
import HsBindgen.Backend.Hs.Haddock.Translation
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Instances qualified as Hs
import HsBindgen.Backend.Hs.Translation.Monad (HsM)
import HsBindgen.Backend.Hs.Translation.Monad qualified as HsM
import HsBindgen.Backend.Hs.Translation.Type qualified as Type
import HsBindgen.Errors
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Translation
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint

-- | Generate declarations for given C struct
structDecs ::
     C.DeclInfo Final
  -> C.Struct Final
  -> PrescriptiveDeclSpec
  -> HsM [Hs.Decl l]
structDecs info struct spec = do
    case struct.flam of
      Nothing   -> getDeclsRegular   spec info struct
      Just flam -> getDeclsFlam flam spec info struct

getDeclsRegular ::
     HasCallStack
  => PrescriptiveDeclSpec
  -> C.DeclInfo Final
  -> C.Struct Final
  -> HsM [Hs.Decl l]
getDeclsRegular spec info struct = do
    env <- Reader.ask
    let supInsts = env.supportedInstances.struct
    insts <-
      getInstances supInsts name struct.fields <$> State.gets (.instanceMap)
    let insts' = Set.insert Inst.Generic insts
        (hsStruct, decls) =
          getDecls supInsts env.haddockConfig spec name info struct insts'
    State.modify' $ #instanceMap %~ Map.insert name hsStruct.instances
    pure $ Hs.DeclData hsStruct : decls
  where
    name :: Hs.Name Hs.NsTypeConstr
    name = Hs.assertNs (Proxy @Hs.NsTypeConstr) info.id.hsName

getDeclsFlam ::
     HasCallStack
  => C.StructField Final
  -> PrescriptiveDeclSpec
  -> C.DeclInfo Final
  -> C.Struct Final
  -> HsM [Hs.Decl l]
getDeclsFlam flam spec info struct = do
    env <- Reader.ask
    let supInsts = env.supportedInstances.struct
    insts <-
      getInstances supInsts auxName struct.fields <$> State.gets (.instanceMap)
    let insts' = insts <> Set.fromList [Inst.Flam_Offset, Inst.Generic]
        (hsStruct, decls) =
          getDecls supInsts env.haddockConfig spec auxName info struct insts'
    State.modify' $ #instanceMap %~ Map.insert auxName hsStruct.instances
    pure $ Hs.DeclData hsStruct : decls ++ [getHasFlamInstanceDecl hsStruct, flamDecl env.haddockConfig]
  where
    name :: Hs.Name Hs.NsTypeConstr
    name = Hs.assertNs (Proxy @Hs.NsTypeConstr) info.id.hsName

    auxName :: Hs.Name Hs.NsTypeConstr
    auxName = case struct.names.flamAux of
        Just n -> n
        Nothing -> panicPure "name of auxiliary declaration unavailable"

    getHasFlamInstanceDecl :: Hs.Struct -> Hs.Decl l
    getHasFlamInstanceDecl hsStruct =
      Hs.DeclDefineInstance
        Hs.DefineInstance{
            comment      = Nothing
          , instanceDecl =
              Hs.InstanceHasFlam hsStruct $
                Hs.HasFlamInstance
                  (Type.topLevel flam.typ)
                  (flam.offset `div` 8)
          }

    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1760>
    -- We generate pointer manipulation bindings for the FLAM field.
    flamDecl :: HaddockConfig -> Hs.Decl l
    flamDecl hCfg =
      Hs.DeclTypSyn
        Hs.TypSyn{
            name
          , typ     =
              Hs.WithFlam
                (Type.topLevel flam.typ)
                (Hs.TypRef auxName Nothing)
          , origin  = Origin.Decl{
              info = info
            , kind = Origin.Opaque info.id.cName.name.kind
            , spec = spec
            }
          , comment = mkHaddocks hCfg info
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
    fieldTypes :: [Hs.Type]
    fieldTypes = Type.topLevel . (.typ) <$> fields

    candidateInsts :: Set Inst.TypeClass
    candidateInsts = Hs.getCandidateInsts supInsts

getDecls ::
     Map Inst.TypeClass Inst.SupportedStrategies
  -> HaddockConfig
  -> PrescriptiveDeclSpec
  -> Hs.Name Hs.NsTypeConstr
  -> C.DeclInfo Final
  -> C.Struct Final
  -> Set Inst.TypeClass
  -> (Hs.Struct, [Hs.Decl l])
getDecls supInsts hCfg spec structName info struct insts =
    ( hsStruct
    , marshalDecls ++ optDecls ++ fieldDecls
    )
  where
    fieldName :: C.StructField Final -> Hs.Name Hs.NsVar
    fieldName = Hs.assertNs (Proxy @Hs.NsVar) . (.info.name.hsName)

    getHsField :: C.StructField Final -> Hs.Field
    getHsField field =
        Hs.Field {
            name    = fieldName field
          , typ     = Type.topLevel field.typ
          , origin  = Origin.StructField field
          , comment = mkHaddocksFieldInfo hCfg info field.info
          }

    fieldHint :: C.StructField Final -> NameHint
    fieldHint = toNameHint . fieldName

    -- Build @\ptr s -> case s of Constr f0 f1 ... -> do { f f0; f f1; ... }@,
    -- shared between 'writeRaw' and 'poke'.
    elimFieldsLambda ::
         (forall ctx. Idx ctx -> C.StructField Final -> Idx ctx -> a ctx)
      -> Hs.Lambda (Hs.Lambda (Hs.ElimStruct (Hs.Seq a))) EmptyCtx
    elimFieldsLambda mkField =
        Hs.Lambda (NameHint "ptr") $ Hs.Lambda (NameHint "s") $
          Vec.reifyList struct.fields $ \fieldsVec ->
            Hs.makeElimStruct IZ hsStruct.constr (fmap fieldHint fieldsVec) $
              \wk xs -> Hs.Seq $ Vec.toList $
                Vec.zipWith (mkField (weaken wk I1)) fieldsVec xs

    hsStruct :: Hs.Struct
    hsStruct = Hs.Struct {
          name      = structName
        , constr    = struct.names.constr
        , fields    = map getHsField struct.fields
        , instances = insts <> knownInsts
        , comment   = mkHaddocks hCfg info
        , origin    = Just Origin.Decl{
              info
            , kind = Origin.Struct struct
            , spec
            }
        }

    marshalDecls :: [Hs.Decl l]
    marshalDecls =
      let hasStaticSize = Inst.StaticSize `Set.member` insts
          hasReadRaw    = Inst.ReadRaw    `Set.member` insts
          hasWriteRaw   = Inst.WriteRaw   `Set.member` insts
          hasStorable   = Inst.Storable   `Set.member` insts
          justWhen p x  = if p then Just x else Nothing
      in  catMaybes [
              justWhen hasStaticSize $
                Hs.DeclDefineInstance Hs.DefineInstance{
                    comment      = Nothing
                  , instanceDecl =
                      Hs.InstanceStaticSize structName Hs.StaticSizeInstance{
                          staticSizeOf    = struct.sizeof
                        , staticAlignment = struct.alignment
                        }
                  }
            , justWhen hasReadRaw $
                Hs.DeclDefineInstance Hs.DefineInstance{
                    comment      = Nothing
                  , instanceDecl =
                      Hs.InstanceReadRaw hsStruct Hs.ReadRawInstance{
                          readRaw = Hs.Lambda (NameHint "ptr") $
                            Hs.Ap (Hs.StructCon hsStruct) $
                              map (readRawField IZ) struct.fields
                        }
                  }
            , justWhen hasWriteRaw $
                Hs.DeclDefineInstance Hs.DefineInstance{
                    comment      = Nothing
                  , instanceDecl =
                      Hs.InstanceWriteRaw hsStruct Hs.WriteRawInstance{
                          writeRaw = elimFieldsLambda writeRawField
                        }
                  }
            , if hasStaticSize && hasReadRaw && hasWriteRaw
                then Just $ Hs.DeclDeriveInstance Hs.DeriveInstance{
                    strategy = Hs.DeriveVia (Hs.EquivStorable (Hs.TypRef structName Nothing))
                  , clss     = Inst.Storable
                  , name     = structName
                  , comment  = Nothing
                  }
                else justWhen hasStorable $
                  Hs.DeclDefineInstance Hs.DefineInstance {
                      comment      = Nothing
                    , instanceDecl = Hs.InstanceStorable hsStruct Hs.StorableInstance{
                          sizeOf    = struct.sizeof
                        , alignment = struct.alignment
                        , peek      = Hs.Lambda (NameHint "ptr") $
                            Hs.Ap (Hs.StructCon hsStruct) $
                              map (peekField IZ) struct.fields
                        , poke      = elimFieldsLambda pokeField
                        }
                    }
            ]

    optDecls :: [Hs.Decl l]
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

    fieldDecls :: [Hs.Decl l]
    fieldDecls = concatMap (getFieldDecls structName struct hsStruct) struct.fields

    knownInsts :: Set Inst.TypeClass
    knownInsts = Set.fromList $ catMaybes [
        if any (isJust . (.width)) struct.fields
          then Just Inst.HasCBitfield
          else Nothing
      , if any (isNothing . (.width)) struct.fields
          then Just Inst.HasCField
          else Nothing
      , if null struct.fields then Nothing else Just Inst.HasField
      , if null struct.fields then Nothing else Just Inst.CompatHasField
      ]

{-------------------------------------------------------------------------------
  Fields
-------------------------------------------------------------------------------}

-- | 'HsBindgen.Runtime.HasCField.HasCField',
-- 'HsBindgen.Runtime.HasCBitfield.HasCBitfield', 'GHC.Records.HasField', and
-- 'GHC.Records.Compat.HasField' instances for a field of a struct declaration
--
-- Given a struct:
--
-- > struct myStruct { int x; char y };
--
-- We generate roughly this datatype:
--
-- > newtype MyStruct = MyStruct { myStruct_x :: CInt, myStruct_y :: CChar }
--
-- GHC automatically generates 'GHC.Records.HasField' instances for the the two
-- fields.
--
-- Then, 'getFieldDecls' will generate roughly the following class instances
-- for the fields @x@ and @y@ respectively:
--
-- > instance HasCField "myStruct_x" MyStruct where
-- >   type CFieldType "myStruct_x" MyStruct = CInt
-- > instance GHC.Records.HasField "myStruct_x" (Ptr MyStruct) (Ptr CInt)
-- > instance GHC.Records.Compat.HasField "myStruct_x" MyStruct CInt
--
-- > instance HasCField "myStruct_y" MyStruct where
-- >  type CFieldType "myStruct_y" MyStruct = CChar
-- > instance GHC.Records.HasField "myStruct_y" (Ptr MyStruct) (Ptr CChar)
-- > instance GHC.Records.Compat.HasField "myStruct_y" MyStruct CChar
--
-- This works similarly for bit-fields, but those get a
-- 'HsBindgen.Runtime.HasCBitfield.HasCBitfield' instance instead of a
-- 'HsBindgen.Runtime.HasCField.HasCField' instance.
getFieldDecls ::
     Hs.Name Hs.NsTypeConstr
  -> C.Struct Final
  -> Hs.Struct
  -> C.StructField Final
  -> [Hs.Decl l]
getFieldDecls structName cStruct hsStruct field = [
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
    , Hs.DeclDefineInstance $
        Hs.DefineInstance {
            comment      = Nothing
          , instanceDecl = Hs.InstanceCompatHasField compatHasFieldDecl
          }
    ]
  where
    parentType :: Hs.Type
    parentType = Hs.TypRef structName Nothing

    fieldName :: Hs.Name Hs.NsVar
    fieldName = Hs.assertNs (Proxy @Hs.NsVar) field.info.name.hsName

    fieldType :: Hs.Type
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

    compatHasFieldDecl :: Hs.CompatHasFieldInstance
    compatHasFieldDecl = Hs.CompatHasFieldInstance {
          parentType = parentType
        , fieldName  = fieldName
        , fieldType  = fieldType
        , otherFields = otherFields
        , constr = hsStruct.constr
        }
      where
        -- All fields that are /not/ the field we are creating an instance for
        -- should stay unmodified
        otherFields = flip mapMaybe cStruct.fields $ \field' ->
            let fieldName' = Hs.assertNs (Proxy @Hs.NsVar) field'.info.name.hsName in
            if fieldName == fieldName' then Nothing else Just fieldName'

peekField :: Idx ctx -> C.StructField Final -> Hs.PeekCField ctx
peekField ptr field = case field.width of
    Nothing -> Hs.PeekCField    (Hs.StrLit name) ptr
    Just _w -> Hs.PeekCBitfield (Hs.StrLit name) ptr
  where
    name = Text.unpack field.info.name.hsName.text

pokeField :: Idx ctx -> C.StructField Final -> Idx ctx -> Hs.PokeCField ctx
pokeField ptr field x = case field.width of
    Nothing  -> Hs.PokeCField    (Hs.StrLit name) ptr x
    Just _w  -> Hs.PokeCBitfield (Hs.StrLit name) ptr x
  where
    name = Text.unpack field.info.name.hsName.text

readRawField :: Idx ctx -> C.StructField Final -> Hs.ReadRawCField ctx
readRawField ptr field = case field.width of
    Nothing -> Hs.ReadRawCField    (Hs.StrLit name) ptr
    Just _w -> Hs.ReadRawCBitfield (Hs.StrLit name) ptr
  where
    name = Text.unpack field.info.name.hsName.text

writeRawField ::
     Idx ctx
  -> C.StructField Final
  -> Idx ctx
  -> Hs.WriteRawCField ctx
writeRawField ptr field x = case field.width of
    Nothing  -> Hs.WriteRawCField    (Hs.StrLit name) ptr x
    Just _w  -> Hs.WriteRawCBitfield (Hs.StrLit name) ptr x
  where
    name = Text.unpack field.info.name.hsName.text
