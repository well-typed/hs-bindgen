module HsBindgen.Backend.Hs.Translation.Union (
    unionDecs
  ) where

import Control.Monad.State qualified as State hiding (MonadState)
import Data.Set qualified as Set

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.Haddock.Config (HaddockConfig)
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Haddock.Translation
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Instances qualified as Hs
import HsBindgen.Backend.Hs.Translation.Newtype qualified as Hs
import HsBindgen.Backend.Hs.Translation.State (HsM, TranslationState)
import HsBindgen.Backend.Hs.Translation.State qualified as State
import HsBindgen.Backend.Hs.Translation.Type qualified as Type
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.MangleNames.IsPass qualified as MangleNames
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
import HsBindgen.Language.Haskell qualified as Hs

unionDecs ::
     HaddockConfig
  -> C.DeclInfo Final
  -> C.Union Final
  -> PrescriptiveDeclSpec
  -> HsM [Hs.Decl]
unionDecs haddockConfig info union spec = do
    nt <- newtypeDec
    flip aux nt <$> State.get
  where
    newtypeDec :: HsM Hs.Newtype
    newtypeDec =
        Hs.newtypeDec newtypeName newtypeConstr newtypeField
          newtypeOrigin newtypeComment candidateInsts knownInsts
      where
        newtypeName :: Hs.Name Hs.NsTypeConstr
        newtypeName = Hs.unsafeHsIdHsName info.id.unsafeHsName

        newtypeConstr :: Hs.Name Hs.NsConstr
        newtypeConstr = union.names.constr

        newtypeField :: Hs.Field
        newtypeField = Hs.Field {
              name    = union.names.field
            , typ     = Hs.HsByteArray
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
        newtypeComment = mkHaddocks haddockConfig info newtypeName

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
    aux :: TranslationState -> Hs.Newtype -> [Hs.Decl]
    aux transState nt =
        Hs.DeclNewtype nt : marshalDecls ++ accessorDecls ++
        concatMap (unionFieldDecls nt.name) union.fields
      where
        marshalDecls :: [Hs.Decl]
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
                strategy = Hs.DeriveVia (HsEquivStorable (Hs.HsTypRef nt.name Nothing))
              , clss     = Inst.Storable
              , name     = nt.name
              , comment  = Nothing
              }
          ]

        sba :: Hs.HsType
        sba =
            HsSizedByteArray
              (fromIntegral union.sizeof)
              (fromIntegral union.alignment)

        accessorDecls :: [Hs.Decl]
        accessorDecls = concatMap getAccessorDecls union.fields

        getAccessorDecls :: C.UnionField Final -> [Hs.Decl]
        getAccessorDecls field =
            if Inst.Storable `Set.member` fInsts
              then [
                  Hs.DeclUnionGetter Hs.UnionGetter{
                      name    = getterName
                    , typ     = hsType
                    , constr  = nt.name
                    , comment = mkHaddocksFieldInfo haddockConfig info field.info
                             <> commentRefName (Hs.getName setterName)
                    }
                , Hs.DeclUnionSetter Hs.UnionSetter{
                      name    = setterName
                    , typ     = hsType
                    , constr  = nt.name
                    , comment = commentRefName (Hs.getName getterName)
                    }
                ]
              else []
          where
            hsType :: HsType
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

-- | 'HasCField' and 'HasField' instances for a field of a
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
-- This works similarly for bit-fields, but those get a 'HasCBitfield' instance
-- instead of a 'HasCField' instance.
unionFieldDecls :: Hs.Name Hs.NsTypeConstr -> C.UnionField Final -> [Hs.Decl]
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

    parentType :: HsType
    parentType = Hs.HsTypRef unionName Nothing

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
