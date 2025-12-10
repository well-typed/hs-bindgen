module HsBindgen.Frontend.Pass.HandleTypedefs (handleTypedefs) where

import Data.Map.Strict qualified as Map

import Clang.HighLevel.Documentation qualified as Clang

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.Typedefs (TypedefAnalysis)
import HsBindgen.Frontend.Analysis.Typedefs qualified as TypedefAnalysis
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.HandleTypedefs.IsPass
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

handleTypedefs ::
     C.TranslationUnit Select
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
    td = TypedefAnalysis.fromDecls unitAnn.declDeclUse unitDecls

    msgs   :: [Maybe (Msg HandleTypedefs)]
    decls' :: [Maybe (C.Decl HandleTypedefs)]
    (msgs, decls') = second (concatMap sequence)
                   $ unzip
                   $ map (handleDecl td) unitDecls

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

-- | Strip pointer indirection to find function type
--
-- Examples:
--
-- * @TypePointer (TypeFun args res)@ returns @Just (args, res, 1)@
-- * @TypePointer (TypePointer (TypeFun args res))@ returns @Just (args, res, 2)@
-- * @TypePointer (TypePointer (TypePointer (TypeFun args res)))@ returns @Just (args, res, 3)@
-- * @TypePointer TypeVoid@ returns @Nothing@
--
-- This handles arbitrary levels of pointer indirection (N >= 1).
--
stripPointersToFunction :: C.Type p -> Maybe ([C.Type p], C.Type p, Int)
stripPointersToFunction = go 1
  where
    go :: Int -> C.Type p -> Maybe ([C.Type p], C.Type p, Int)
    go !n (C.TypePointer inner) =
      case inner of
        C.TypeFun args res -> Just (args, res, n)
        C.TypePointer _    -> go (n + 1) inner  -- Recurse through more pointers
        _                  -> Nothing
    go _ _ = Nothing

handleDecl ::
     TypedefAnalysis
  -> C.Decl Select
  -> (Maybe (Msg HandleTypedefs), Maybe [C.Decl HandleTypedefs])
handleDecl td decl
  -- Deal with typedefs around function pointers (N levels of indirection)
  -- (NOTE: Such typedefs are never squashed.)
  -- See issue #1380
  | C.DeclTypedef dtd <- declKind
  , Just (args, res, n) <- stripPointersToFunction (C.typedefType dtd)
  = ( Nothing
    , Just $ introduceAuxFunType td declInfo' declAnn n args res
    )

  -- Check for typedefs we need to squash
  | Just _ty <- Map.lookup dId td.squash
  = ( Just $ HandleTypedefsSquashed declInfo'
    , Nothing
    )

  -- Check for types we need to rename
  --
  -- We emit a \"rename\" only if we truly are renaming (in some cases we
  -- just change some other properties of the identifier).
  | Just newDeclId <- Map.lookup dId td.rename
  = ( do guard $ declInfo'.declId.name /= newDeclId.name
         Just $ HandleTypedefsRenamedTagged
                  declInfo'
                  newDeclId.name
    , Just . (:[]) $ C.Decl{
           declInfo = declInfo'{C.declId = newDeclId}
         , declKind = handleUseSites td declKind
         , declAnn
        }
    )

  | otherwise
  = ( Nothing
    , Just . (:[]) $ C.Decl{
           declInfo = declInfo'
         , declKind = handleUseSites td declKind
         , declAnn
        }
    )

  where
    C.Decl{
        declInfo = declInfo@C.DeclInfo{declId = dId}
      , declKind
      , declAnn
      } = decl

    declInfo' :: C.DeclInfo HandleTypedefs
    declInfo' = coercePass declInfo

-- | Introduce auxiliary type for typedef around function pointer
--
-- Given
--
-- > typedef void (*Foo)(int x);
--
-- we generate
--
-- > newtype Foo_Deref = Foo_Deref (CInt -> IO ())
-- > newtype Foo       = Foo       (FunPtr Foo_Deref)
--
-- For multi-level pointers:
--
-- > typedef void (**Foo)(int x);
--
-- we generate
--
-- > newtype Foo_Deref = Foo_Deref (CInt -> IO ())
-- > newtype Foo       = Foo       (Ptr (FunPtr Foo_Deref))
--
introduceAuxFunType ::
     TypedefAnalysis
  -> C.DeclInfo HandleTypedefs
  -> Ann "Decl" HandleTypedefs
  -> Int                    -- ^ Number of indirection layers that  this
                            -- function type had
  -> [C.Type Select]         -- ^ Function arguments
  -> C.Type Select           -- ^ Function result type
  -> [C.Decl HandleTypedefs]
introduceAuxFunType td declInfo declAnn n args res = [
      derefDecl
    , mainDecl
    ]
  where
    derefDecl, mainDecl :: C.Decl HandleTypedefs
    derefDecl = C.Decl {
          declInfo = declInfo {
              C.declId = C.DeclId{
                  name       = declInfo.declId.name <> "_Deref"
                , nameKind   = C.NameKindOrdinary
                , haskellId  = ()
                , origDeclId =
                    case declInfo.declId.origDeclId of
                      C.OrigDeclId orig    -> C.AuxForDecl orig
                      C.AuxForDecl _parent ->
                        -- Aux decls are not introduced until @HandleTypedefs@
                        panicPure "Unexpected aux decl"
                }
            , C.declComment = Just auxType
            }
        , declKind = handleUseSites td
                   $ C.DeclTypedef $ C.Typedef {
                       typedefType = C.TypeFun args res
                     , typedefAnn  = NoAnn
                     }
        , declAnn  = def
        }
    mainDecl = C.Decl {
          C.declInfo = declInfo
        , C.declKind = C.DeclTypedef $ C.Typedef {
            typedefType =
              -- Reconstruct all pointer layers around the TypeTypedef
              -- For single pointer: TypePointer (TypeTypedef ...)
              -- For double pointer: TypePointer (TypePointer (TypeTypedef ...))
              -- etc.
              let baseType = C.TypeTypedef
                               (C.declId $ C.declInfo derefDecl)
                               (handleUseSites td $ C.TypeFun args res)
                  pointerLayers = replicate n C.TypePointer
               in foldr ($) baseType pointerLayers
          , typedefAnn  = NoAnn
          }
        , C.declAnn = declAnn
        }

    auxType :: C.Comment HandleTypedefs
    auxType = C.Comment $
        Clang.Comment [
          Clang.Paragraph [
              Clang.TextContent "Auxiliary type used by "
            , Clang.InlineRefCommand $
                C.CommentRef declInfo.declId.name Nothing
            ]
        ]

{-------------------------------------------------------------------------------
  Use sites
-------------------------------------------------------------------------------}

class HandleUseSites a where
  handleUseSites :: TypedefAnalysis -> a Select -> a HandleTypedefs

instance HandleUseSites C.DeclKind where
  handleUseSites td = \case
      C.DeclStruct struct    -> C.DeclStruct (handleUseSites td struct)
      C.DeclUnion union      -> C.DeclUnion (handleUseSites td union)
      C.DeclEnum enum        -> C.DeclEnum (handleUseSites td enum)
      C.DeclTypedef typedef  -> C.DeclTypedef (handleUseSites td typedef)
      C.DeclOpaque cNameKind -> C.DeclOpaque cNameKind
      C.DeclMacro macro      -> C.DeclMacro (handleUseSites td (coercePass macro))
      C.DeclFunction fun     -> C.DeclFunction (handleUseSites td fun)
      C.DeclGlobal ty        -> C.DeclGlobal (handleUseSites td ty)

instance HandleUseSites C.Struct where
  handleUseSites td C.Struct{..} = C.Struct{
        structFields = map (handleUseSites td) structFields
      , ..
      }

instance HandleUseSites C.StructField where
  handleUseSites td C.StructField{..} = C.StructField{
        structFieldInfo = coercePass structFieldInfo
      , structFieldType = handleUseSites td structFieldType
      , ..
      }

instance HandleUseSites C.Union where
  handleUseSites td C.Union{..} = C.Union{
        unionFields = map (handleUseSites td) unionFields
      , ..
      }

instance HandleUseSites C.UnionField where
  handleUseSites td C.UnionField{..} = C.UnionField{
        unionFieldInfo = coercePass unionFieldInfo
      , unionFieldType = handleUseSites td unionFieldType
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
        functionArgs = map (bimap id (handleUseSites td)) functionArgs
      , functionRes  = handleUseSites td functionRes
      , ..
      }

instance HandleUseSites C.Type where
  handleUseSites td = go
    where
      go :: C.Type Select -> C.Type HandleTypedefs

      -- Trivial cases

      go (C.TypePrim prim)      = C.TypePrim prim
      go (C.TypeComplex prim)   = C.TypeComplex prim
      go (C.TypeVoid)           = C.TypeVoid
      go (C.TypeExtBinding ext) = C.TypeExtBinding ext

      -- Recursive cases

      go (C.TypePointer ty)         = C.TypePointer (go ty)
      go (C.TypeFun args res)       = C.TypeFun (map go args) (go res)
      go (C.TypeConstArray n ty)    = C.TypeConstArray n (go ty)
      go (C.TypeIncompleteArray ty) = C.TypeIncompleteArray (go ty)
      go (C.TypeBlock ty)           = C.TypeBlock (go ty)
      go (C.TypeConst ty)           = C.TypeConst (go ty)

      -- Interesting cases: tagged types may be renamed, typedefs may be squashed

      go (C.TypeRef     uid)     = rename uid
      go (C.TypeTypedef uid uTy) = squash uid uTy

      rename :: C.DeclId Select -> C.Type HandleTypedefs
      rename old = C.TypeRef $
        case Map.lookup old td.rename of
          Nothing  -> coercePass old
          Just new -> new

      squash :: C.DeclId Select -> C.Type Select -> C.Type HandleTypedefs
      squash old uTy =
          case Map.lookup old td.squash of
            Nothing  -> C.TypeTypedef (coercePass old) (go uTy)
            Just new -> C.TypeTypedef new              (go uTy)

