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
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Haddock.Translation
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Field
import HsBindgen.Backend.Hs.Translation.Instances qualified as Hs
import HsBindgen.Backend.Hs.Translation.Monad (HsM)
import HsBindgen.Backend.Hs.Translation.Monad qualified as HsM
import HsBindgen.Backend.Hs.Translation.Type qualified as Type
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
      C.NoFlam ->
        getDeclsRegular spec info struct
      C.Flam flam flamNames ->
        getDeclsFlam flam flamNames.aux spec info struct

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
          getDecls supInsts env spec name info struct insts'
    State.modify' $ #instanceMap %~ Map.insert name hsStruct.instances
    pure $ Hs.DeclData hsStruct : decls
  where
    name :: Hs.Name Hs.NsTypeConstr
    name = Hs.assertNs (Proxy @Hs.NsTypeConstr) info.id.hsName

getDeclsFlam ::
     HasCallStack
  => C.ExplicitField Final
  -> Hs.Name Hs.NsTypeConstr -- ^ Auxiliary type-constructor name
  -> PrescriptiveDeclSpec
  -> C.DeclInfo Final
  -> C.Struct Final
  -> HsM [Hs.Decl l]
getDeclsFlam flam auxName spec info struct = do
    env <- Reader.ask
    let supInsts = env.supportedInstances.struct
    insts <-
      getInstances supInsts auxName struct.fields <$> State.gets (.instanceMap)
    let insts' = insts <> Set.fromList [Inst.Flam_Offset, Inst.Generic]
        (hsStruct, decls) =
          getDecls supInsts env spec auxName info struct insts'
    State.modify' $ #instanceMap %~ Map.insert auxName hsStruct.instances
    pure $ Hs.DeclData hsStruct : decls ++ [getHasFlamInstanceDecl hsStruct, flamDecl env.haddockConfig]
  where
    name :: Hs.Name Hs.NsTypeConstr
    name = Hs.assertNs (Proxy @Hs.NsTypeConstr) info.id.hsName

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
  -> [C.Field Final]
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
  -> HsM.Env
  -> PrescriptiveDeclSpec
  -> Hs.Name Hs.NsTypeConstr
  -> C.DeclInfo Final
  -> C.Struct Final
  -> Set Inst.TypeClass
  -> (Hs.Struct, [Hs.Decl l])
getDecls supInsts env spec structName info struct insts =
    ( hsStruct
    , marshalDecls ++ optDecls ++ isStructDecl ++ fieldDecls
    )
  where
    fieldName :: C.Field Final -> Hs.Name Hs.NsVar
    fieldName = Hs.assertNs (Proxy @Hs.NsVar) . (.info.name.hsName)

    getHsField :: C.Field Final -> Hs.Field
    getHsField field =
        Hs.Field {
            name    = fieldName field
          , typ     = Type.topLevel field.typ
          , origin  = Origin.StructField field
          , comment = mkHaddocksFieldInfo env.haddockConfig info field.info
          }

    fieldHint :: C.Field Final -> NameHint
    fieldHint = toNameHint . fieldName

    -- Build @\ptr s -> case s of Constr f0 f1 ... -> do { f f0; f f1; ... }@,
    -- shared between 'writeRaw' and 'poke'.
    elimFieldsLambda ::
         (forall ctx. Idx ctx -> C.Field Final -> Idx ctx -> a ctx)
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
        , comment   = mkHaddocks env.haddockConfig info
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
    fieldDecls = flip concatMap (flattenFields struct.fields) $ \field -> concat [
          hasFieldDecs env info hsStruct field
        , hasFieldCompatDecs env info hsStruct field
        , hasFieldPtrDecs hsStruct field
        , hasCFieldDecs hsStruct field
        , hasCBitfieldDecs hsStruct field
        ]

    isStructDecl :: [Hs.Decl l]
    isStructDecl = [
          Hs.DeclDeriveInstance Hs.DeriveInstance{
              strategy = Hs.DeriveVia (Hs.IsStructViaStorable (Hs.TypRef hsStruct.name Nothing))
            , clss     = Inst.IsStruct
            , name     = hsStruct.name
            , comment  = Nothing
            }
        | Inst.IsStruct `elem` hsStruct.instances
        ]

    knownInsts :: Set Inst.TypeClass
    knownInsts = Set.fromList $ catMaybes [
        if any (isJust . (.width)) struct.fields
          then Just Inst.HasCBitfield
          else Nothing
      , if any (isNothing . (.width)) struct.fields
          then Just Inst.HasCField
          else Nothing
      , if null struct.fields then Nothing else Just Inst.HasField
      , if null struct.fields then Nothing else Just Inst.HasFieldPtr
      , if null struct.fields then Nothing else Just Inst.HasFieldCompat
        -- TODO <https://github.com/well-typed/hs-bindgen/issues/1447>: this is
        -- actually not a "known" instance because it has a superclass
        -- constraint, but the backend instance resolution isn't strong enough
        -- to resolve this for us
      , if Inst.Storable `elem` insts then Just Inst.IsStruct else Nothing
      ]

{-------------------------------------------------------------------------------
  HasField
-------------------------------------------------------------------------------}

-- | Class instances for 'GHC.Records.HasField'
hasFieldDecs ::
     HsM.Env
  -> C.DeclInfo Final
  -> Hs.Struct
  -> Field
  -> [Hs.Decl l]
hasFieldDecs env info struct field = case field of
    -- Explicit and implicit fields are translated to Haskell record datatype
    -- fields, so they get @HasField@ instances
    ExplicitField _ -> []
    ImplicitField _ -> []
    -- Indirect fields are not translated to Haskell record datatype fields, so
    -- they get custom @HasField@ instances composed from existing @HasField@
    -- instances
    IndirectField impField indField -> auxIndirectField impField indField
  where
    fieldComment :: Maybe HsDoc.Comment
    fieldComment = mkHaddocksFieldInfo env.haddockConfig info (getFieldInfo field)

    parentType :: Hs.Type
    parentType = Hs.TypRef struct.name Nothing

    fieldName :: Hs.Name Hs.NsVar
    fieldName = Hs.assertNs (Proxy @Hs.NsVar) (getFieldInfo field).name.hsName

    fieldType :: Hs.Type
    fieldType = Type.topLevel (getFieldTyp field)

    auxIndirectField :: C.ImplicitField Final -> C.IndirectField Final -> [Hs.Decl l]
    auxIndirectField impField indField = [
          Hs.DeclDefineInstance $
            Hs.DefineInstance {
                comment      = fieldComment
              , instanceDecl = Hs.InstanceHasField decl
              }
        | Inst.HasField `elem` struct.instances
        ]
      where
        decl :: Hs.HasFieldInstance
        decl = Hs.HasFieldInstance {
              parentType = parentType
            , fieldName  = fieldName
            , fieldType  = fieldType
            , impl = impl
            }

        impl :: Hs.HasFieldImpl
        impl = Hs.HasFieldImplIndirect {
              nameTopToAnon = Hs.assertNs (Proxy @Hs.NsVar) impField.info.name.hsName
            , nameAnonToTarget = Hs.assertNs (Proxy @Hs.NsVar) indField.ann.fieldNameInAnon.hsName
            }

-- | Class instances for 'GHC.Records.Compat.HasField'
hasFieldCompatDecs ::
     HsM.Env
  -> C.DeclInfo Final
  -> Hs.Struct
  -> Field
  -> [Hs.Decl l]
hasFieldCompatDecs env info struct field = [
      Hs.DeclDefineInstance $
        Hs.DefineInstance {
            comment      = fieldComment
          , instanceDecl = Hs.InstanceHasFieldCompat decl
          }
    | Inst.HasFieldCompat `elem` struct.instances
    ]
  where
    fieldComment :: Maybe HsDoc.Comment
    fieldComment = mkHaddocksFieldInfo env.haddockConfig info (getFieldInfo field)

    parentType :: Hs.Type
    parentType = Hs.TypRef struct.name Nothing

    fieldName :: Hs.Name Hs.NsVar
    fieldName = Hs.assertNs (Proxy @Hs.NsVar) (getFieldInfo field).name.hsName

    fieldType :: Hs.Type
    fieldType = Type.topLevel (getFieldTyp field)

    decl :: Hs.HasFieldCompatInstance
    decl = Hs.HasFieldCompatInstance {
          parentType = parentType
        , fieldName  = fieldName
        , fieldType  = fieldType
        , impl = impl
        }

    impl, implRecord :: Hs.HasFieldCompatImpl
    impl = case field of
            ExplicitField _ -> implRecord
            ImplicitField _ -> implRecord
            IndirectField impF indF -> implIndirect impF indF

    implRecord = Hs.HasFieldCompatImplRecord {
          otherFields = otherFields
        , constr = struct.constr
        }
      where
        -- All fields that are /not/ the field we are creating an instance for
        -- should stay unmodified
        otherFields = flip mapMaybe struct.fields $ \field' ->
            let fieldName' = field'.name in
            if fieldName == fieldName' then Nothing else Just fieldName'

    implIndirect :: C.ImplicitField Final -> C.IndirectField Final -> Hs.HasFieldCompatImpl
    implIndirect impField indField = Hs.HasFieldCompatImplIndirect {
          nameTopToAnon = Hs.assertNs (Proxy @Hs.NsVar) impField.info.name.hsName
        , nameAnonToTarget = Hs.assertNs (Proxy @Hs.NsVar) indField.ann.fieldNameInAnon.hsName
        }

-- | Class instances for 'GHC.Records.HasField' for the pointer manipulation API
hasFieldPtrDecs :: Hs.Struct -> Field -> [Hs.Decl l]
hasFieldPtrDecs struct field =
    [ Hs.DeclDefineInstance $
        Hs.DefineInstance {
            comment      = Nothing
          , instanceDecl = Hs.InstanceHasFieldPtr hasFieldPtrDecl
          }
    | Inst.HasFieldPtr `elem` struct.instances
    ]
  where
    parentType :: Hs.Type
    parentType = Hs.TypRef struct.name Nothing

    fieldName :: Hs.Name Hs.NsVar
    fieldName = Hs.assertNs (Proxy @Hs.NsVar) (getFieldInfo field).name.hsName

    fieldType :: Hs.Type
    fieldType = Type.topLevel (getFieldTyp field)

    hasFieldPtrDecl :: Hs.HasFieldPtrInstance
    hasFieldPtrDecl = Hs.HasFieldPtrInstance {
          parentType = parentType
        , fieldName  = fieldName
        , fieldType  = fieldType
        , deriveVia  =
            case getFieldWidth field of
              Nothing -> Hs.ViaHasCField
              Just _  -> Hs.ViaHasCBitfield
        }

-- | Class instances for 'HsBindgen.Runtime.HasCField.HasCField'
hasCFieldDecs :: Hs.Struct -> Field -> [Hs.Decl l]
hasCFieldDecs struct field = case getFieldWidth field of
    Just _w -> []
    Nothing ->
      [ Hs.DeclDefineInstance $
          Hs.DefineInstance {
              comment      = Nothing
            , instanceDecl = Hs.InstanceHasCField decl
            }
      | Inst.HasCField `elem` struct.instances
      ]
  where
    parentType :: Hs.Type
    parentType = Hs.TypRef struct.name Nothing

    fieldName :: Hs.Name Hs.NsVar
    fieldName = Hs.assertNs (Proxy @Hs.NsVar) (getFieldInfo field).name.hsName

    fieldType :: Hs.Type
    fieldType = Type.topLevel (getFieldTyp field)

    decl :: Hs.HasCFieldInstance
    decl = Hs.HasCFieldInstance {
          parentType  = parentType
        , fieldName   = fieldName
        , cFieldType  = fieldType
        , fieldOffset = getFieldOffset field `div` 8
        }

-- | Class instances for 'HsBindgen.Runtime.HasCBitfield.HasCBitfield'
hasCBitfieldDecs :: Hs.Struct -> Field -> [Hs.Decl l]
hasCBitfieldDecs struct field = case getFieldWidth field of
    Nothing -> []
    Just w ->
      [ Hs.DeclDefineInstance $
          Hs.DefineInstance {
              comment      = Nothing
            , instanceDecl = Hs.InstanceHasCBitfield $ decl w
            }
      | Inst.HasCBitfield `elem` struct.instances
      ]
  where
    parentType :: Hs.Type
    parentType = Hs.TypRef struct.name Nothing

    fieldName :: Hs.Name Hs.NsVar
    fieldName = Hs.assertNs (Proxy @Hs.NsVar) (getFieldInfo field).name.hsName

    fieldType :: Hs.Type
    fieldType = Type.topLevel (getFieldTyp field)

    decl :: Int -> Hs.HasCBitfieldInstance
    decl w = Hs.HasCBitfieldInstance {
          parentType    = parentType
        , fieldName     = fieldName
        , cBitfieldType = fieldType
        , bitOffset     = getFieldOffset field
        , bitWidth      = w
        }

{-------------------------------------------------------------------------------
  Field I\/O
-------------------------------------------------------------------------------}

peekField :: Idx ctx -> C.Field Final -> Hs.PeekCField ctx
peekField ptr field = case field.width of
    Nothing -> Hs.PeekCField    (Hs.StrLit name) ptr
    Just _w -> Hs.PeekCBitfield (Hs.StrLit name) ptr
  where
    name = Text.unpack field.info.name.hsName.text

pokeField :: Idx ctx -> C.Field Final -> Idx ctx -> Hs.PokeCField ctx
pokeField ptr field x = case field.width of
    Nothing  -> Hs.PokeCField    (Hs.StrLit name) ptr x
    Just _w  -> Hs.PokeCBitfield (Hs.StrLit name) ptr x
  where
    name = Text.unpack field.info.name.hsName.text

readRawField :: Idx ctx -> C.Field Final -> Hs.ReadRawCField ctx
readRawField ptr field = case field.width of
    Nothing -> Hs.ReadRawCField    (Hs.StrLit name) ptr
    Just _w -> Hs.ReadRawCBitfield (Hs.StrLit name) ptr
  where
    name = Text.unpack field.info.name.hsName.text

writeRawField ::
     Idx ctx
  -> C.Field Final
  -> Idx ctx
  -> Hs.WriteRawCField ctx
writeRawField ptr field x = case field.width of
    Nothing  -> Hs.WriteRawCField    (Hs.StrLit name) ptr x
    Just _w  -> Hs.WriteRawCBitfield (Hs.StrLit name) ptr x
  where
    name = Text.unpack field.info.name.hsName.text
