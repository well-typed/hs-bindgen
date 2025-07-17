module HsBindgen.Frontend.Pass.HandleTypedefs (handleTypedefs) where

import Data.Map.Strict qualified as Map

import HsBindgen.Frontend.Analysis.Typedefs (TypedefAnalysis)
import HsBindgen.Frontend.Analysis.Typedefs qualified as TypedefAnalysis
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleTypedefs.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass
import HsBindgen.Frontend.Pass.Sort.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

handleTypedefs ::
     C.TranslationUnit ResolveBindingSpec
  -> (C.TranslationUnit HandleTypedefs, [Msg HandleTypedefs])
handleTypedefs C.TranslationUnit{..} = (
      C.TranslationUnit{
          unitDecls = catMaybes decls'
        , ..
        }
    , catMaybes msgs
    )
  where
    td :: TypedefAnalysis
    td = TypedefAnalysis.fromDecls (declDeclUse unitAnn) unitDecls

    msgs   :: [Maybe (Msg HandleTypedefs)]
    decls' :: [Maybe (C.Decl HandleTypedefs)]
    (msgs, decls') = unzip $ map (handleDecl td) unitDecls

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

handleDecl ::
     TypedefAnalysis
  -> C.Decl ResolveBindingSpec
  -> (Maybe (Msg HandleTypedefs), Maybe (C.Decl HandleTypedefs))
handleDecl td decl =
    case declKind of
      C.DeclTypedef{} ->
        case Map.lookup curName (TypedefAnalysis.squash td) of
          Just _ty -> (
              Just $ HandleTypedefsSquashed declInfo'
            , Nothing
            )
          Nothing -> (
              Nothing
            , Just C.Decl{
                  declInfo = declInfo'
                , declKind = handleUseSites td declKind
                , declAnn
                }
            )
      _otherwise ->
        let (mMsg, updatedInfo) =
               case Map.lookup curName (TypedefAnalysis.rename td) of
                 Nothing -> (Nothing, declInfo')
                 Just (newName, newOrigin) -> (
                     Just $ HandleTypedefsRenamedTagged declInfo' newName
                   , declInfo {
                       C.declId = C.DeclId newName newOrigin
                     }
                   )
        in ( mMsg
           , Just C.Decl{
                  declInfo = updatedInfo
                , declKind = handleUseSites td declKind
                , declAnn
               }
           )
  where
    C.Decl{
        declInfo = declInfo@C.DeclInfo{declId = C.DeclId{declIdName = curName}}
      , declKind
      , declAnn
      } = decl

    declInfo' :: C.DeclInfo HandleTypedefs
    declInfo' = coercePass declInfo

{-------------------------------------------------------------------------------
  Use sites
-------------------------------------------------------------------------------}

class HandleUseSites a where
  handleUseSites :: TypedefAnalysis -> a ResolveBindingSpec -> a HandleTypedefs

instance HandleUseSites C.DeclKind where
  handleUseSites td = \case
      C.DeclStruct struct   -> C.DeclStruct (handleUseSites td struct)
      C.DeclStructOpaque    -> C.DeclStructOpaque
      C.DeclUnion union     -> C.DeclUnion (handleUseSites td union)
      C.DeclUnionOpaque     -> C.DeclUnionOpaque
      C.DeclEnum enum       -> C.DeclEnum (handleUseSites td enum)
      C.DeclEnumOpaque      -> C.DeclEnumOpaque
      C.DeclTypedef typedef -> C.DeclTypedef (handleUseSites td typedef)
      C.DeclMacro macro     -> C.DeclMacro (handleUseSites td macro)
      C.DeclFunction fun    -> C.DeclFunction (handleUseSites td fun)
      C.DeclGlobal ty       -> C.DeclGlobal (handleUseSites td ty)
      C.DeclConst ty        -> C.DeclConst (handleUseSites td ty)

instance HandleUseSites C.Struct where
  handleUseSites td C.Struct{..} = C.Struct{
        structFields = map (handleUseSites td) structFields
      , ..
      }

instance HandleUseSites C.StructField where
  handleUseSites td C.StructField{..} = C.StructField{
        structFieldType = handleUseSites td structFieldType
      , ..
      }

instance HandleUseSites C.Union where
  handleUseSites td C.Union{..} = C.Union{
        unionFields = map (handleUseSites td) unionFields
      , ..
      }

instance HandleUseSites C.UnionField where
  handleUseSites td C.UnionField{..} = C.UnionField{
        unionFieldType = handleUseSites td unionFieldType
      , ..
      }

instance HandleUseSites C.Enum where
  handleUseSites td C.Enum{..} = C.Enum{
        enumType      = handleUseSites td enumType
      , enumConstants = map coercePass enumConstants
      , ..
      }

instance HandleUseSites C.Typedef where
  handleUseSites td C.Typedef{..} = C.Typedef{
        typedefType = handleUseSites td typedefType
      , ..
      }

instance HandleUseSites C.CheckedMacro where
  handleUseSites td (C.MacroType typ)  = C.MacroType (handleUseSites td typ)
  handleUseSites _  (C.MacroExpr expr) = C.MacroExpr expr

instance HandleUseSites C.CheckedMacroType where
  handleUseSites td C.CheckedMacroType{..} = C.CheckedMacroType{
        macroType = handleUseSites td macroType
      , ..
      }

instance HandleUseSites C.Function where
  handleUseSites td C.Function{..} = C.Function{
        functionArgs = map (handleUseSites td) functionArgs
      , functionRes  = handleUseSites td functionRes
      , ..
      }

instance HandleUseSites C.Type where
  handleUseSites td = go
    where
      go :: C.Type ResolveBindingSpec -> C.Type HandleTypedefs

      -- Simple cases

      go (C.TypePrim prim)        = C.TypePrim prim
      go (C.TypeMacroTypedef uid) = C.TypeMacroTypedef uid
      go (C.TypeVoid)             = C.TypeVoid
      go (C.TypeExtBinding ext)   = C.TypeExtBinding ext

      -- Recursive cases

      go (C.TypePointer ty)         = C.TypePointer (go ty)
      go (C.TypeFun args res)       = C.TypeFun (map go args) (go res)
      go (C.TypeConstArray n ty)    = C.TypeConstArray n (go ty)
      go (C.TypeIncompleteArray ty) = C.TypeIncompleteArray (go ty)
      go (C.TypeBlock ty)           = C.TypeBlock (go ty)

      -- Interesting cases: tagged types may be renamed, typedefs may be squashed

      go (C.TypeStruct uid) = rename C.TypeStruct uid
      go (C.TypeUnion  uid) = rename C.TypeUnion  uid
      go (C.TypeEnum   uid) = rename C.TypeEnum   uid

      go (C.TypeTypedef name) = squash name

      rename ::
           (C.DeclId -> C.Type HandleTypedefs)
        -> (C.DeclId -> C.Type HandleTypedefs)
      rename mkType uid@C.DeclId{..} =
        case Map.lookup declIdName (TypedefAnalysis.rename td) of
          Just (newName, newOrigin) -> mkType $ C.DeclId newName newOrigin
          Nothing                   -> mkType uid

      squash :: C.Name -> C.Type HandleTypedefs
      squash name = C.TypeTypedef $
          case Map.lookup name (TypedefAnalysis.squash td) of
            Nothing -> TypedefRegular $ C.DeclId name C.NameOriginInSource
            Just ty -> TypedefSquashed name ty
