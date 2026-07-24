module HsBindgen.Backend.Hs.Translation.Union (
    unionDecs
  ) where

import Control.Monad.Reader qualified as Reader
import Control.Monad.State qualified as State hiding (MonadState)
import Data.Set qualified as Set

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Haddock.Translation
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Field
import HsBindgen.Backend.Hs.Translation.Instances qualified as Hs
import HsBindgen.Backend.Hs.Translation.Monad (HsM)
import HsBindgen.Backend.Hs.Translation.Monad qualified as HsM
import HsBindgen.Backend.Hs.Translation.Newtype qualified as Hs
import HsBindgen.Backend.Hs.Translation.Type qualified as Type
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.MangleNames.IsPass qualified as MangleNames
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Translation
import HsBindgen.Language.Haskell qualified as Hs

unionDecs ::
     HasCallStack
  => C.DeclInfo Final
  -> C.Union Final
  -> PrescriptiveDeclSpec
  -> HsM [Hs.Decl l]
unionDecs info union spec = do
    st <- State.get
    env <- Reader.ask
    nt <- newtypeDec env
    pure $ aux st env nt
  where
    newtypeDec :: HsM.Env -> HsM Hs.Newtype
    newtypeDec env = do
        Hs.newtypeDec newtypeName newtypeConstr newtypeField
          newtypeOrigin newtypeComment candidateInsts knownInsts
      where
        newtypeName :: Hs.Name Hs.NsTypeConstr
        newtypeName = Hs.assertNs (Proxy @Hs.NsTypeConstr) info.id.hsName

        newtypeConstr :: Hs.Name Hs.NsConstr
        newtypeConstr = union.names.dataConstr

        newtypeField :: Hs.Field
        newtypeField = Hs.Field {
              name    = union.names.field
            , typ     = Hs.ByteArray
            , origin  = Origin.GeneratedField
            , comment = Nothing
            }

        newtypeOrigin :: Origin.Decl Origin.Newtype
        newtypeOrigin =  Origin.Decl {
              info = info
            , kind = Origin.Union union
            , spec = spec
            }

        newtypeComment :: Maybe HsDoc.Comment
        newtypeComment = mkHaddocks env.haddockConfig info

        candidateInsts :: Set Inst.TypeClass
        candidateInsts = Set.empty

        knownInsts :: Set Inst.TypeClass
        knownInsts = Set.fromList $ catMaybes [
            Just Inst.Generic
          , if any (isJust . (.width)) union.fields
              then Just Inst.HasCBitfield
              else Nothing
          , if any (isNothing . (.width)) union.fields
              then Just Inst.HasCField
              else Nothing
          , if null union.fields then Nothing else Just Inst.HasField
          , if null union.fields then Nothing else Just Inst.HasFieldCompat
          , if null union.fields then Nothing else Just Inst.HasFieldPtr
          , Just Inst.IsUnion
          , Just Inst.ReadRaw
          , Just Inst.StaticSize
          , Just Inst.Storable
          , Just Inst.WriteRaw
          ]

    -- everything in aux is state-dependent
    aux :: HsM.St -> HsM.Env -> Hs.Newtype -> [Hs.Decl l]
    aux st env nt =
        Hs.DeclNewtype nt : marshalDecls ++ isUnionDecl ++
        fieldDecls
      where
        marshalDecls :: [Hs.Decl l]
        marshalDecls = [
            Hs.DeclDeriveInstance Hs.DeriveInstance{
                strategy = Hs.DeriveStock
              , clss     = Inst.Generic
              , name     = nt.name
              , comment  = Nothing
              }
          , Hs.DeclDeriveInstance Hs.DeriveInstance{
                strategy = Hs.DeriveVia sba
              , clss     = Inst.StaticSize
              , name     = nt.name
              , comment  = Nothing
              }
          , Hs.DeclDeriveInstance Hs.DeriveInstance{
                strategy = Hs.DeriveVia sba
              , clss     = Inst.ReadRaw
              , name     = nt.name
              , comment  = Nothing
              }
          , Hs.DeclDeriveInstance Hs.DeriveInstance{
                strategy = Hs.DeriveVia sba
              , clss     = Inst.WriteRaw
              , name     = nt.name
              , comment  = Nothing
              }
          , Hs.DeclDeriveInstance Hs.DeriveInstance{
                strategy = Hs.DeriveVia (Hs.EquivStorable (Hs.TypRef nt.name Nothing))
              , clss     = Inst.Storable
              , name     = nt.name
              , comment  = Nothing
              }
          ]

        sba :: Hs.Type
        sba =
            Hs.SizedByteArray
              (fromIntegral union.sizeof)
              (fromIntegral union.alignment)

        fieldDecls :: [Hs.Decl l]
        fieldDecls = flip concatMap (flattenFields union.fields) $ \field -> concat [
            hasFieldDecs st env info nt field
          , hasFieldCompatDecs st env info nt field
          , hasFieldPtrDecs nt field
          , hasCFieldDecs nt field
          , hasCBitfieldDecs nt field
          ]

        isUnionDecl :: [Hs.Decl l]
        isUnionDecl = [
              Hs.DeclDeriveInstance Hs.DeriveInstance{
                  strategy = Hs.DeriveVia sba
                , clss     = Inst.IsUnion
                , name     = nt.name
                , comment  = Nothing
                }
            | Inst.IsUnion `Set.member` nt.instances
            ]

{-------------------------------------------------------------------------------
  HasField
-------------------------------------------------------------------------------}

-- | Class instances for 'GHC.Records.HasField'
hasFieldDecs ::
     HsM.St
  -> HsM.Env
  -> C.DeclInfo Final
  -> Hs.Newtype
  -> Field
  -> [Hs.Decl l]
hasFieldDecs st env info union field =
    [ Hs.DeclDefineInstance $
        Hs.DefineInstance {
            comment = fieldComment
          , instanceDecl = Hs.InstanceHasField decl
          }
    | Inst.HasField `elem` union.instances
    , Inst.Storable `elem` fieldInsts
    ]
  where
    fieldInsts :: Set Inst.TypeClass
    fieldInsts = Hs.getInstances
                    st.instanceMap
                    (Just union.name)
                    (Set.singleton Inst.Storable)
                    [fieldType]

    fieldComment :: Maybe HsDoc.Comment
    fieldComment = mkHaddocksFieldInfo env.haddockConfig info (getFieldInfo field)

    parentType :: Hs.Type
    parentType = Hs.TypRef union.name Nothing

    fieldName :: Hs.Name Hs.NsVar
    fieldName = Hs.assertNs (Proxy @Hs.NsVar) (getFieldInfo field).name.hsName

    fieldType :: Hs.Type
    fieldType = Type.topLevel (getFieldTyp field)

    decl :: Hs.HasFieldInstance
    decl = Hs.HasFieldInstance {
          parentType = parentType
        , fieldName  = fieldName
        , fieldType  = fieldType
        , impl       = impl
        }

    impl, implUnion :: Hs.HasFieldImpl
    impl = case field of
        -- Unions are not translated to Haskell record datatypes. Therefore
        -- explicit and implicit fields are not translated to Haskell record
        -- datatype fields either. Instead of getting @HasField@ instances for
        -- free we have to define custom instances.
        ExplicitField _ -> implUnion
        ImplicitField _ -> implUnion
        -- Indirect fields are not translated to Haskell record datatype fields, so
        -- they get custom @HasField@ instances composed from existing @HasField@
        -- instances
        IndirectField impField indField -> implIndirect impField indField

    implUnion = Hs.HasFieldImplUnion

    implIndirect :: C.ImplicitField Final -> C.IndirectField Final -> Hs.HasFieldImpl
    implIndirect impField indField  = Hs.HasFieldImplIndirect {
          nameTopToAnon = Hs.assertNs (Proxy @Hs.NsVar) impField.info.name.hsName
        , nameAnonToTarget = Hs.assertNs (Proxy @Hs.NsVar) indField.ann.fieldNameInAnon.hsName
        }

-- | Class instances for 'GHC.Records.Compat.HasField'
hasFieldCompatDecs ::
     HsM.St
  -> HsM.Env
  -> C.DeclInfo Final
  -> Hs.Newtype
  -> Field
  -> [Hs.Decl l]
hasFieldCompatDecs st env info union field =
   [ Hs.DeclDefineInstance $
        Hs.DefineInstance {
            comment = fieldComment
          , instanceDecl = Hs.InstanceHasFieldCompat decl
          }
    | Inst.HasFieldCompat `elem` union.instances
    , Inst.Storable `elem` fieldInsts
    ]
  where
    fieldInsts :: Set Inst.TypeClass
    fieldInsts = Hs.getInstances
                    st.instanceMap
                    (Just union.name)
                    (Set.singleton Inst.Storable)
                    [fieldType]

    fieldComment :: Maybe HsDoc.Comment
    fieldComment = mkHaddocksFieldInfo env.haddockConfig info (getFieldInfo field)

    parentType :: Hs.Type
    parentType = Hs.TypRef union.name Nothing

    fieldName :: Hs.Name Hs.NsVar
    fieldName = Hs.assertNs (Proxy @Hs.NsVar) (getFieldInfo field).name.hsName

    fieldType :: Hs.Type
    fieldType = Type.topLevel (getFieldTyp field)

    decl :: Hs.HasFieldCompatInstance
    decl = Hs.HasFieldCompatInstance {
          parentType = parentType
        , fieldName  = fieldName
        , fieldType  = fieldType
        , impl       = impl
        }

    impl, implUnion :: Hs.HasFieldCompatImpl
    impl = case field of
        -- Unions are not translated to Haskell record datatypes. Therefore
        -- explicit and implicit fields are not translated to Haskell record
        -- datatype fields either. Instead of getting @HasField@ instances for
        -- free we have to define custom instances.
        ExplicitField _ -> implUnion
        ImplicitField _ -> implUnion
        -- Indirect fields are not translated to Haskell record datatype fields, so
        -- they get custom @HasField@ instances composed from existing @HasField@
        -- instances
        IndirectField impField indField -> implIndirect impField indField

    implUnion = Hs.HasFieldCompatImplUnion

    implIndirect :: C.ImplicitField Final -> C.IndirectField Final -> Hs.HasFieldCompatImpl
    implIndirect impField indField  = Hs.HasFieldCompatImplIndirect {
          nameTopToAnon = Hs.assertNs (Proxy @Hs.NsVar) impField.info.name.hsName
        , nameAnonToTarget = Hs.assertNs (Proxy @Hs.NsVar) indField.ann.fieldNameInAnon.hsName
        }

-- | Class instances for 'GHC.Records.HasField' for the pointer manipulation API
hasFieldPtrDecs :: Hs.Newtype -> Field -> [Hs.Decl l]
hasFieldPtrDecs union field =
    [ Hs.DeclDefineInstance $
        Hs.DefineInstance {
            comment      = Nothing
          , instanceDecl = Hs.InstanceHasFieldPtr decl
          }
    | Inst.HasFieldPtr `elem` union.instances
    ]
  where
    parentType :: Hs.Type
    parentType = Hs.TypRef union.name Nothing

    fieldName :: Hs.Name Hs.NsVar
    fieldName = Hs.assertNs (Proxy @Hs.NsVar) (getFieldInfo field).name.hsName

    fieldType :: Hs.Type
    fieldType = Type.topLevel (getFieldTyp field)

    decl :: Hs.HasFieldPtrInstance
    decl = Hs.HasFieldPtrInstance {
          parentType = parentType
        , fieldName  = fieldName
        , fieldType  = fieldType
        , deriveVia  =
            case getFieldWidth field of
              Nothing -> Hs.ViaHasCField
              Just _  -> Hs.ViaHasCBitfield
        }

-- | Class instances for 'HsBindgen.Runtime.HasCField.HasCField'
hasCFieldDecs :: Hs.Newtype -> Field -> [Hs.Decl l]
hasCFieldDecs union field = case getFieldWidth field of
    Just _w -> []
    Nothing ->
      [ Hs.DeclDefineInstance $
          Hs.DefineInstance {
              comment      = Nothing
            , instanceDecl = Hs.InstanceHasCField decl
            }
      | Inst.HasCField `elem` union.instances
      ]
  where
    parentType :: Hs.Type
    parentType = Hs.TypRef union.name Nothing

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
hasCBitfieldDecs :: Hs.Newtype -> Field -> [Hs.Decl l]
hasCBitfieldDecs union field = case getFieldWidth field of
    Nothing -> []
    Just w ->
      [ Hs.DeclDefineInstance $
          Hs.DefineInstance {
              comment      = Nothing
            , instanceDecl = Hs.InstanceHasCBitfield $ decl w
            }
      | Inst.HasCBitfield `elem` union.instances
      ]
  where
    parentType :: Hs.Type
    parentType = Hs.TypRef union.name Nothing

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
