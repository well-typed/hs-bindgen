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
          -- TODO <https://github.com/well-typed/hs-bindgen/issues/1253>
          -- Should correctly detect 'Inst.HasCBitfield' and 'Inst.HasCField'
          -- when bit-fields in unions are supported.
          , if null union.fields then Nothing else Just Inst.HasCField
          , if null union.fields then Nothing else Just Inst.HasField
          , Just Inst.ReadRaw
          , Just Inst.StaticSize
          , Just Inst.Storable
          , Just Inst.WriteRaw
          ]

    -- everything in aux is state-dependent
    aux :: HsM.St -> HsM.Env -> Hs.Newtype -> [Hs.Decl l]
    aux transState env nt =
        Hs.DeclNewtype nt : marshalDecls ++ accessorDecls ++
        concatMap (unionFieldDecls nt.name) union.fields
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

        accessorDecls :: [Hs.Decl l]
        accessorDecls = concatMap getAccessorDecls union.fields

        getAccessorDecls :: C.UnionField Final -> [Hs.Decl l]
        getAccessorDecls field =
            if Inst.Storable `Set.member` fInsts
              then [
                  Hs.DeclUnionGetter Hs.UnionGetter{
                      name    = getterName
                    , typ     = hsType
                    , constr  = nt.name
                    , comment = mkHaddocksFieldInfo env.haddockConfig info field.info
                             <> commentRefName setterName.text
                    }
                , Hs.DeclUnionSetter Hs.UnionSetter{
                      name    = setterName
                    , typ     = hsType
                    , constr  = nt.name
                    , comment = commentRefName getterName.text
                    }
                ]
              else []
          where
            hsType :: Hs.Type
            hsType     = Type.topLevel field.typ
            fInsts     = Hs.getInstances
                            transState.instanceMap
                            (Just nt.name)
                            (Set.singleton Inst.Storable)
                            [hsType]

            getterName, setterName :: Hs.Name Hs.NsVar
            getterName = field.names.getter
            setterName = field.names.setter

            commentRefName :: Text -> Maybe HsDoc.Comment
            commentRefName name = Just $ HsDoc.paragraph [
                HsDoc.Bold [HsDoc.TextContent "See:"]
              , HsDoc.Identifier name
              ]

-- | 'HsBindgen.Runtime.HasCField.HasCField' and 'GHC.Records.HasField' instances for a field of a
-- union declaration
--
-- Given a union:
--
-- > union myUnion { int option1; char option2 };
--
-- We generate roughly this newtype:
--
-- > newtype MyUnion = MyUnion { unwrapMyUnion :: ByteArray }
--
-- Then, 'unionFieldDecls' will generate roughly the following class instances
-- for the fields @option1@ and @option@ respectively:
--
-- > instance HasCField "myUnion_option1" MyUnion where
-- >   type CFieldType "myUnion_option1" MyUnion = CInt
-- > instance HasField "myUnion_option1" (Ptr MyUnion) (Ptr CInt)
--
-- > instance HasCField "myUnion_option2" MyUnion where
-- >   type CFieldType "myUnion_option2" MyUnion = CChar
-- > instance HasField "myUnion_option2" (Ptr MyUnion) (Ptr CChar)
--
-- This works similarly for bit-fields, but those get a 'HsBindgen.Runtime.HasCBitfield.HasCBitfield' instance
-- instead of a 'HsBindgen.Runtime.HasCField.HasCField' instance.
unionFieldDecls :: Hs.Name Hs.NsTypeConstr -> C.UnionField Final -> [Hs.Decl l]
unionFieldDecls unionName field = [
      Hs.DeclDefineInstance $
        Hs.DefineInstance {
            comment      = Nothing
          , instanceDecl =
              case unionFieldWidth field of
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
    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1253>
    -- Should be changed to @C.unionFieldWidth f@ when bit-fields in unions are
    -- supported.
    unionFieldWidth :: C.UnionField Final -> Maybe Int
    unionFieldWidth _f = Nothing

    parentType :: Hs.Type
    parentType = Hs.TypRef unionName Nothing

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
