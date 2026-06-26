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
import HsBindgen.Backend.Hs.Translation.Instances qualified as Hs
import HsBindgen.Backend.Hs.Translation.Monad (HsM)
import HsBindgen.Backend.Hs.Translation.Monad qualified as HsM
import HsBindgen.Backend.Hs.Translation.Newtype qualified as Hs
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.MangleNames.IsPass qualified as MangleNames
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.Translation qualified as Translation
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
          -- TODO <https://github.com/well-typed/hs-bindgen/issues/1253>
          -- Should correctly detect 'Inst.HasCBitfield' and 'Inst.HasCField'
          -- when bit-fields in unions are supported.
          , if null union.fields then Nothing else Just Inst.HasCField
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
        fieldDecls = flip concatMap union.fields $ \field ->
          hasFieldDecs st env info nt field ++
          hasFieldPtrDecs nt field

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


-- | Class instances for 'GHC.Records.HasField' and
-- 'GHC.Records.Compat.HasField' for a union field
--
-- Given a union:
--
-- > union myUnion { int option1; char option2 };
--
-- We generate roughly this newtype:
--
-- > newtype MyUnion = MyUnion { unwrapMyUnion :: ByteArray }
--
-- Then, 'hasFieldDecs' will generate roughly the following class instances for
-- the fields @option1@ and @option@ respectively:
--
-- > instance GHC.Records.HasField "myUnion_option1" MyUnion CInt
-- > instance GHC.Records.Compat.HasField "myUnion_option1" MyUnion CInt
--
-- > instance GHC.Records.HasField "myUnion_option2"  MyUnion CChar
-- > instance GHC.Records.Compat.HasField "myUnion_option2" MyUnion CChar
--
-- Unions do not get 'GHC.Records.HasField' instances for its fields for free
-- (like with structs), so we generate them instead in addition to
-- 'GHC.Records.Compat.HasField' instances.
--
hasFieldDecs ::
     HsM.St
  -> HsM.Env
  -> C.DeclInfo Final
  -> Hs.Newtype
  -> C.UnionField Final
  -> [Hs.Decl l]
hasFieldDecs st env info nt field =
    if Inst.Storable `Set.member` fInsts
    then concat [
        [ Hs.DeclDefineInstance $
            Hs.DefineInstance {
                comment = fieldComment
              , instanceDecl = Hs.InstanceHasField baseHasFieldDecl
              }
        | Inst.HasField `elem` nt.instances
        ]
      , [ Hs.DeclDefineInstance $
            Hs.DefineInstance {
                comment = fieldComment
              , instanceDecl = Hs.InstanceHasFieldCompat compatHasFieldDecl
              }
        | Inst.HasFieldCompat `elem` nt.instances
        ]
      ]
    else []
  where
    fInsts     = Hs.getInstances
                    st.instanceMap
                    (Just nt.name)
                    (Set.singleton Inst.Storable)
                    [fieldType]

    fieldComment :: Maybe HsDoc.Comment
    fieldComment = mkHaddocksFieldInfo env.haddockConfig info field.info

    parentType :: Hs.Type
    parentType = Hs.TypRef nt.name Nothing

    fieldName :: Hs.Name Hs.NsVar
    fieldName = Hs.assertNs (Proxy @Hs.NsVar) field.info.name.hsName

    fieldType :: Hs.Type
    fieldType = Translation.topLevel field.typ.c

    baseHasFieldDecl :: Hs.HasFieldInstance
    baseHasFieldDecl = Hs.HasFieldInstance {
          parentType = parentType
        , fieldName  = fieldName
        , fieldType  = fieldType
        , impl       = Hs.HasFieldImplUnion
        }

    compatHasFieldDecl :: Hs.HasFieldCompatInstance
    compatHasFieldDecl = Hs.HasFieldCompatInstance {
          parentType = parentType
        , fieldName  = fieldName
        , fieldType  = fieldType
        , impl       = Hs.HasFieldCompatImplUnion
        }

-- | Class instances for 'GHC.Records.HasField' for a union field for the
-- pointer manipulation API
--
-- Given a union:
--
-- > union myUnion { int option1; char option2 };
--
-- We generate roughly this newtype:
--
-- > newtype MyUnion = MyUnion { unwrapMyUnion :: ByteArray }
--
-- Then, 'hasFieldPtrDecs' will generate roughly the following class instances
-- for the fields @option1@ and @option@ respectively:
--
-- > instance HasCField "myUnion_option1" MyUnion where
-- >   type CFieldType "myUnion_option1" MyUnion = CInt
-- > instance GHC.Records.HasField "myUnion_option1" (Ptr MyUnion) (Ptr CInt)
--
-- > instance HasCField "myUnion_option2" MyUnion where
-- >   type CFieldType "myUnion_option2" MyUnion = CChar
-- > instance GHC.Records.HasField "myUnion_option2" (Ptr MyUnion) (Ptr CChar)
--
-- This works similarly for bit-fields, but those get a
-- 'HsBindgen.Runtime.HasCBitfield.HasCBitfield' instance instead of a
-- 'HsBindgen.Runtime.HasCField.HasCField' instance.
--
hasFieldPtrDecs :: Hs.Newtype -> C.UnionField Final -> [Hs.Decl l]
hasFieldPtrDecs nt field = concat [
      [ Hs.DeclDefineInstance $
          Hs.DefineInstance {
              comment      = Nothing
            , instanceDecl = Hs.InstanceHasFieldPtr hasFieldPtrDecl
            }
      | Inst.HasFieldPtr `elem` nt.instances
      ]
    , [ Hs.DeclDefineInstance $
          Hs.DefineInstance {
              comment      = Nothing
            , instanceDecl =
                case unionFieldWidth field of
                  Nothing -> Hs.InstanceHasCField $ hasCFieldDecl
                  Just w  -> Hs.InstanceHasCBitfield $ hasCBitfieldDecl w
            }
      | case unionFieldWidth field of
          Nothing -> Inst.HasCField `elem` nt.instances
          Just{} -> Inst.HasCBitfield `elem` nt.instances
      ]
    ]
  where
    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1253>
    -- Should be changed to @C.unionFieldWidth f@ when bit-fields in unions are
    -- supported.
    unionFieldWidth :: C.UnionField Final -> Maybe Int
    unionFieldWidth _f = Nothing

    parentType :: Hs.Type
    parentType = Hs.TypRef nt.name Nothing

    fieldName :: Hs.Name Hs.NsVar
    fieldName = Hs.assertNs (Proxy @Hs.NsVar) field.info.name.hsName

    fieldType :: Hs.Type
    fieldType = Translation.topLevel field.typ.c

    hasFieldPtrDecl :: Hs.HasFieldPtrInstance
    hasFieldPtrDecl = Hs.HasFieldPtrInstance {
          parentType = parentType
        , fieldName  = fieldName
        , fieldType  = fieldType
        , deriveVia  =
            case unionFieldWidth field of
              Nothing -> Hs.ViaHasCField
              Just _  -> Hs.ViaHasCBitfield
        }

    hasCFieldDecl :: Hs.HasCFieldInstance
    hasCFieldDecl = Hs.HasCFieldInstance {
          parentType  = parentType
        , fieldName   = fieldName
        , cFieldType  = fieldType
        , fieldOffset = 0
        }

    hasCBitfieldDecl :: Int -> Hs.HasCBitfieldInstance
    hasCBitfieldDecl w = Hs.HasCBitfieldInstance {
          parentType    = parentType
        , fieldName     = fieldName
        , cBitfieldType = fieldType
        , bitOffset     = 0
        , bitWidth      = w
        }
