module HsBindgen.Frontend.Pass.HandleTypedefs (handleTypedefs) where

import Data.Map.Strict qualified as Map

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass.HandleTypedefs.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

handleTypedefs ::
     C.TranslationUnit ResolveBindingSpec
  -> C.TranslationUnit HandleTypedefs
handleTypedefs C.TranslationUnit{..} = C.TranslationUnit{
      unitDecls = mapMaybe (handleDecl td) unitDecls
    , ..
    }
  where
    td :: Typedefs
    td = analyseTypedefs unitDecls

{-------------------------------------------------------------------------------
  Analysis

  TODO: This is not quite right (#589)
-------------------------------------------------------------------------------}

-- Which typedefs should be squashed?
type Typedefs = Map CName (Type HandleTypedefs)

analyseTypedefs :: [Decl ResolveBindingSpec] -> Typedefs
analyseTypedefs = Map.fromList . mapMaybe squash

-- | Should we squash this declaration?
squash :: C.Decl ResolveBindingSpec -> Maybe (CName, Type HandleTypedefs)
squash C.Decl{declInfo = C.DeclInfo{declId}, declKind} =
    case declKind of
      C.DeclTypedef typedef ->
        (declId,) <$> squashTypedef declId typedef
      _otherwise ->
        Nothing

-- | Should this /declaration/ of a typedef be squashed?
--
-- If so, we return its underlying type.
squashTypedef ::
     CName
  -> Typedef ResolveBindingSpec
  -> Maybe (C.Type HandleTypedefs)
squashTypedef typedefName C.Typedef{typedefType = typ} =
    case typ of
      C.TypeStruct n o -> guard (n == typedefName) >> return (C.TypeStruct n o)
      C.TypeUnion  n o -> guard (n == typedefName) >> return (C.TypeUnion  n o)
      C.TypeEnum   n o -> guard (n == typedefName) >> return (C.TypeEnum   n o)
      _otherwise     -> Nothing

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

handleDecl :: Typedefs -> Decl ResolveBindingSpec -> Maybe (Decl HandleTypedefs)
handleDecl td decl = do
    case declKind of
      C.DeclTypedef{} ->
        guard $ isNothing (Map.lookup (declId declInfo) td)
      _otherwise ->
        return ()
    return Decl{
        declInfo = coercePass declInfo
      , declKind = handleUseSites td declKind
      , declAnn
      }
  where
    Decl{declInfo, declKind, declAnn} = decl

{-------------------------------------------------------------------------------
  Use sites
-------------------------------------------------------------------------------}

class HandleUseSites a where
  handleUseSites :: Typedefs -> a ResolveBindingSpec -> a HandleTypedefs

instance HandleUseSites DeclKind where
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

      go (C.TypePrim prim)                = C.TypePrim prim
      go (C.TypeStruct name origin)       = C.TypeStruct name origin
      go (C.TypeUnion name origin)        = C.TypeUnion name origin
      go (C.TypeEnum name origin)         = C.TypeEnum name origin
      go (C.TypeMacroTypedef name origin) = C.TypeMacroTypedef name origin
      go (C.TypeVoid)                     = C.TypeVoid
      go (C.TypeExtBinding cSpelling extHsRef typeSpec) =
          C.TypeExtBinding cSpelling extHsRef typeSpec

      -- Recursive cases

      go (C.TypePointer ty)         = C.TypePointer (go ty)
      go (C.TypeFun args res)       = C.TypeFun (map go args) (go res)
      go (C.TypeConstArray n ty)    = C.TypeConstArray n (go ty)
      go (C.TypeIncompleteArray ty) = C.TypeIncompleteArray (go ty)

      -- The actual typedef case

      go (C.TypeTypedef name) = C.TypeTypedef $
          case Map.lookup name td of
            Nothing -> TypedefRegular  name
            Just ty -> TypedefSquashed name ty
