module HsBindgen.Frontend.Pass.ReparseMacroExpansions (
    reparseMacroExpansions
  ) where

import Control.Monad.Reader (MonadReader (..), Reader, runReader)
import Control.Monad.State (MonadState, StateT, modify, runStateT)
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set

import Clang.CStandard (ClangCStandard)

import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.LanguageC qualified as LanC
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

type CType = C.Type ReparseMacroExpansions

-- | Reparse declarations that have macro expansions in their type positions,
-- using the known-types environment from 'typecheckMacros'.
reparseMacroExpansions ::
     ClangCStandard
  -> LanC.ReparseEnv TypecheckMacros
     -- ^ Known non-macro type (see 'ReparseEnv')
  -> LanC.ReparseEnv TypecheckMacros
     -- ^ Known macro type names (see 'ReparseEnv')
  -> C.TranslationUnit TypecheckMacros
  -> C.TranslationUnit ReparseMacroExpansions
reparseMacroExpansions cStd knownNonMacroTypes knownMacroTypes unit =
    let (reparsedDecls, reparseState) =
          runM
            cStd
            (Map.map coercePass knownNonMacroTypes)
            (Map.map coercePass knownMacroTypes)
            (mapM reparseDecl unit.decls)
    in reconstructAfterReparse unit reparseState reparsedDecls

{-------------------------------------------------------------------------------
  Reconstruct translation unit after reparsing
-------------------------------------------------------------------------------}

reconstructAfterReparse ::
     C.TranslationUnit TypecheckMacros
  -> ReparseState
  -> [C.Decl ReparseMacroExpansions]
  -> C.TranslationUnit ReparseMacroExpansions
reconstructAfterReparse unit reparseState decls =
    C.TranslationUnit{
        decls        = decls
      , includeGraph = unit.includeGraph
      , ann          = unit.ann{
            declIndex    = updatedDeclIndex
          , useDeclGraph = updatedUseDeclGraph
          , declUseGraph = DeclUseGraph.fromUseDecl updatedUseDeclGraph
          }
      }
  where
    isSuccessfulReparse :: C.Decl ReparseMacroExpansions -> Bool
    isSuccessfulReparse d = Set.member d.info.id reparseState.reparseSuccesses

    successfulReparses :: [C.Decl ReparseMacroExpansions]
    successfulReparses = List.filter isSuccessfulReparse decls

    updatedDeclIndex :: DeclIndex
    updatedDeclIndex =
      -- We use @foldr@ here to establish the original order of reparse
      -- warnings.
      foldr
        DeclIndex.registerDelayedParseMsg
        unit.ann.declIndex
        reparseState.reparseWarnings

    updatedUseDeclGraph :: UseDeclGraph
    updatedUseDeclGraph =
      Foldable.foldl'
        (flip updateDeps)
        unit.ann.useDeclGraph
        successfulReparses

{-------------------------------------------------------------------------------
  Reparse declarations with macro expansions
-------------------------------------------------------------------------------}

reparseDecl ::
     C.Decl TypecheckMacros
  -> M (C.Decl ReparseMacroExpansions)
reparseDecl decl = case decl.kind of
    C.DeclMacro macro                    -> processMacro            info' macro
    C.DeclTypedef typedef                -> processTypedef          info' typedef
    C.DeclStruct struct                  -> processStruct           info' struct
    C.DeclUnion union                    -> processUnion            info' union
    C.DeclEnum enum                      -> processEnum             info' enum
    C.DeclAnonEnumConstant anonEnumConst -> processAnonEnumConstant info' anonEnumConst
    C.DeclOpaque                         -> processOpaque           info' C.DeclOpaque
    C.DeclFunction fun                   -> processFunction         info' fun
    C.DeclGlobal global                  -> processGlobal           info' global
  where
    info' :: C.DeclInfo ReparseMacroExpansions
    info' = coercePass decl.info

{-------------------------------------------------------------------------------
  Function for each kind of declaration
-------------------------------------------------------------------------------}

-- | Macros have already been type-checked; just coerce the pass annotation.
processMacro ::
     C.DeclInfo ReparseMacroExpansions
  -> CheckedMacro TypecheckMacros
  -> M (C.Decl ReparseMacroExpansions)
processMacro info macro =
    pure C.Decl{
        info = info
      , ann  = NoAnn
      , kind = C.DeclMacro (coercePass macro)
      }

processStruct ::
     C.DeclInfo ReparseMacroExpansions
  -> C.Struct TypecheckMacros
  -> M (C.Decl ReparseMacroExpansions)
processStruct info struct = do
    mkDecl
      <$> mapM (processStructField info.id) struct.fields
      <*> mapM (processStructField info.id) struct.flam
  where
    mkDecl ::
         [C.StructField ReparseMacroExpansions]
      -> Maybe (C.StructField ReparseMacroExpansions)
      -> C.Decl ReparseMacroExpansions
    mkDecl fields flam = C.Decl{
          info = info
        , ann  = NoAnn
        , kind = C.DeclStruct C.Struct{
              fields    = fields
            , flam      = flam
            , sizeof    = struct.sizeof
            , alignment = struct.alignment
            , ann       = struct.ann
            }
        }

-- | Reconstruct a 'C.FieldInfo' after reparsing.
--
-- When @mName@ is 'Nothing', the original name is preserved; when it is
-- 'Just', the reparsed name replaces it.
mkFieldInfo ::
     C.FieldInfo TypecheckMacros
  -> Maybe Text
  -> C.FieldInfo ReparseMacroExpansions
mkFieldInfo info mName = C.FieldInfo{
      name    = maybe info.name CScopedName mName
    , comment = fmap coercePass info.comment
    , loc     = info.loc
    }

processStructField ::
     DeclId
  -> C.StructField TypecheckMacros
  -> M (C.StructField ReparseMacroExpansions)
processStructField declId field =
    reparseWith declId LanC.reparseField field.ann withoutReparse withReparse
  where
    withoutReparse :: C.StructField ReparseMacroExpansions
    withoutReparse = C.StructField{
          typ    = coercePass field.typ
        , ann    = NoAnn
        , offset = field.offset
        , width  = field.width
        , info   = mkFieldInfo field.info Nothing
        }

    withReparse :: (CType, Text) -> M (C.StructField ReparseMacroExpansions)
    withReparse (ty, name) = pure C.StructField{
          typ    = ty
        , ann    = NoAnn
        , offset = field.offset
        , width  = field.width
        , info   = mkFieldInfo field.info (Just name)
        }

processUnion ::
     C.DeclInfo ReparseMacroExpansions
  -> C.Union TypecheckMacros
  -> M (C.Decl ReparseMacroExpansions)
processUnion info union = do
    combineFields <$> mapM (processUnionField info.id) union.fields
  where
    combineFields :: [C.UnionField ReparseMacroExpansions] -> C.Decl ReparseMacroExpansions
    combineFields fields = C.Decl{
          info = info
        , ann  = NoAnn
        , kind = C.DeclUnion C.Union{
              fields    = fields
            , sizeof    = union.sizeof
            , alignment = union.alignment
            , ann       = union.ann
            }
        }

processUnionField ::
     DeclId
  -> C.UnionField TypecheckMacros
  -> M (C.UnionField ReparseMacroExpansions)
processUnionField declId field =
    reparseWith declId LanC.reparseField field.ann withoutReparse withReparse
  where
    withoutReparse :: C.UnionField ReparseMacroExpansions
    withoutReparse = C.UnionField{
          typ  = coercePass field.typ
        , ann  = NoAnn
        , info = mkFieldInfo field.info Nothing
        }

    withReparse :: (CType, Text) -> M (C.UnionField ReparseMacroExpansions)
    withReparse (ty, name) = pure C.UnionField{
          typ  = ty
        , ann  = NoAnn
        , info = mkFieldInfo field.info (Just name)
        }

processOpaque ::
     C.DeclInfo ReparseMacroExpansions
  -> C.DeclKind ReparseMacroExpansions
  -> M (C.Decl ReparseMacroExpansions)
processOpaque info kind =
    pure C.Decl{
        info = info
      , kind = kind
      , ann  = NoAnn
      }

processEnum ::
     C.DeclInfo ReparseMacroExpansions
  -> C.Enum TypecheckMacros
  -> M (C.Decl ReparseMacroExpansions)
processEnum info enum =
    mkDecl <$> mapM processEnumConstant enum.constants
  where
    mkDecl :: [C.EnumConstant ReparseMacroExpansions] -> C.Decl ReparseMacroExpansions
    mkDecl enumerators = C.Decl{
          info = info
        , ann  = NoAnn
        , kind = C.DeclEnum C.Enum{
              typ       = coercePass enum.typ
            , constants = enumerators
            , sizeof    = enum.sizeof
            , alignment = enum.alignment
            , ann       = enum.ann
            }
        }

processAnonEnumConstant ::
     C.DeclInfo ReparseMacroExpansions
  -> C.AnonEnumConstant TypecheckMacros
  -> M (C.Decl ReparseMacroExpansions)
processAnonEnumConstant info anonEnumConst =
    mkDecl <$> processEnumConstant anonEnumConst.constant
  where
    mkDecl :: C.EnumConstant ReparseMacroExpansions -> C.Decl ReparseMacroExpansions
    mkDecl constant = C.Decl{
          info = info
        , ann  = NoAnn
        , kind = C.DeclAnonEnumConstant C.AnonEnumConstant{
              typ      = anonEnumConst.typ
            , constant = constant
            }
        }

processEnumConstant ::
     C.EnumConstant TypecheckMacros
  -> M (C.EnumConstant ReparseMacroExpansions)
processEnumConstant constant =
    pure C.EnumConstant{
        value = constant.value
      , info  = C.FieldInfo{
            comment = fmap coercePass constant.info.comment
          , name    = constant.info.name
          , loc     = constant.info.loc
          }
      }

processTypedef ::
     C.DeclInfo ReparseMacroExpansions
  -> C.Typedef TypecheckMacros
  -> M (C.Decl ReparseMacroExpansions)
processTypedef info typedef = do
    -- If the @typedef@ refers to another type, we do not reparse the
    -- typedef, but instead defer reparsing to that other type.
    -- See https://github.com/well-typed/hs-bindgen/issues/707.
    --
    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1382>
    -- We should allow for pointers.
    let reparseInfo = case typedef.typ of
          C.TypeRef _ -> ReparseNotNeeded
          _otherwise  -> typedef.ann
    reparsedType :: CType <-
      reparseWith info.id LanC.reparseTypedef reparseInfo
        (coercePass typedef.typ)
        pure
    pure C.Decl{
        info = info
      , ann  = NoAnn
      , kind = C.DeclTypedef C.Typedef{
                   typ = reparsedType
                 , ann = NoAnn
                 }
      }

processFunction ::
     C.DeclInfo ReparseMacroExpansions
  -> C.Function TypecheckMacros
  -> M (C.Decl ReparseMacroExpansions)
processFunction info function =
    reparseWith info.id LanC.reparseFunDecl function.ann withoutReparse withReparse
  where
    withoutReparse :: C.Decl ReparseMacroExpansions
    withoutReparse = C.Decl{
          info = info
        , ann  = NoAnn
        , kind = C.DeclFunction C.Function{
              args  = map coercePass function.args
            , res   = coercePass function.res
            , attrs = function.attrs
            , ann   = NoAnn
            }
        }

    withReparse ::
         (([(Maybe Text, CType)] , CType) , Text)
      -> M (C.Decl ReparseMacroExpansions)
    withReparse ((tys, ty), _name) = pure C.Decl{
           info = info
         , ann  = NoAnn
         , kind = C.DeclFunction C.Function{
               args  = map (uncurry mkFunctionArg) tys
             , res   = ty
             , attrs = function.attrs
             , ann   = NoAnn
             }
         }

    mkFunctionArg :: Maybe Text -> CType -> C.FunctionArg ReparseMacroExpansions
    mkFunctionArg mname typ = C.FunctionArg{
          name   = CScopedName <$> mname
        , argTyp = C.TypeFunArgF typ NoAnn
        }

-- | Globals (externs or constants)
processGlobal ::
     C.DeclInfo ReparseMacroExpansions
  -> C.Global TypecheckMacros
  -> M (C.Decl ReparseMacroExpansions)
processGlobal info global =
    reparseWith info.id LanC.reparseGlobal global.ann withoutReparse withReparse
  where
    withoutReparse :: C.Decl ReparseMacroExpansions
    withoutReparse = C.Decl{
          info = info
        , ann  = NoAnn
        , kind = C.DeclGlobal C.Global{
              typ = coercePass global.typ
            , ann = NoAnn
            }
        }

    withReparse :: CType -> M (C.Decl ReparseMacroExpansions)
    withReparse ty = pure C.Decl{
          info = info
        , ann  = NoAnn
        , kind = C.DeclGlobal C.Global{
              typ = ty
            , ann = NoAnn
            }
        }

{-------------------------------------------------------------------------------
  Internal: monad used for reparsing
-------------------------------------------------------------------------------}

newtype M a = WrapM { _unwrapM :: StateT ReparseState (Reader ReparseEnv) a }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadReader ReparseEnv
    , MonadState ReparseState
    )

-- | Environment used when reparsing declarations with macro expansions.
data ReparseEnv = ReparseEnv {
      -- | Known non-macro type names (e.g., @typedef@s or @struct@s)
      knownTypes :: LanC.ReparseEnv ReparseMacroExpansions
      -- | Known macro type names; we keep the known macros separate, because we
      --   need to restrict the reparse environment to macros actually
      --   /expanded/.
    , knownMacroTypes :: LanC.ReparseEnv ReparseMacroExpansions
    }

data ReparseState = ReparseState {
      -- | Declarations with successful reparses
      --
      -- Required to update use-decl graph, see 'updateDeps'.
      reparseSuccesses :: Set DeclId
      -- | Delayed parse messages collected during reparse
      --
      -- Stored in reverse order.
    , reparseWarnings :: [(DeclId, DelayedParseMsg)]
    }
  deriving stock (Generic)

runM ::
     ClangCStandard
  -> LanC.ReparseEnv ReparseMacroExpansions
  -> LanC.ReparseEnv ReparseMacroExpansions
  -> M a
  -> (a, ReparseState)
runM cStd knownTypes knownMacroTypes (WrapM ma) = runReader (runStateT ma s) e
  where
    e :: ReparseEnv
    e = ReparseEnv {
        -- Add the initial reparse environment as a fallback (note, 'Map.union'
        -- is left-biased).
        knownTypes      = knownTypes `Map.union` LanC.initReparseEnv cStd
      , knownMacroTypes = knownMacroTypes
      }

    s :: ReparseState
    s = ReparseState{
        reparseSuccesses = Set.empty
      , reparseWarnings  = []
      }

{-------------------------------------------------------------------------------
  Reparsing declarations with macro expansions
-------------------------------------------------------------------------------}

-- | Run reparser if needed; use fallback on failure or if not needed.
--
-- On failure, records @(declId, ParseMacroErrorReparse e)@ in
-- 'ReparseState.reparseWarnings' and uses the fallback value. Note that
-- 'knownTypes' doubles as the language-c parse environment since both have
-- type 'Map Text CType'.
reparseWith ::
     DeclId
  -> LanC.Parser a
  -> ReparseInfo Tokens
  -> r
  -> (a -> M r)
  -> M r
reparseWith declId parser reparseInfo fallback onSuccess = case reparseInfo of
    ReparseNotNeeded ->
      pure fallback
    ReparseNeeded tokens usedMacros -> do
      env <- ask
      let usedMacroTypes :: LanC.ReparseEnv ReparseMacroExpansions
          usedMacroTypes = Map.restrictKeys env.knownMacroTypes usedMacros
          unknownMacros :: Set Text
          unknownMacros = Set.difference usedMacros (Map.keysSet usedMacroTypes)
      forM_ unknownMacros $ \u ->
          modify $ #reparseWarnings %~
            ((declId, ParseMacroReparseUnknownType u) :)

      let reparseEnv :: LanC.ReparseEnv ReparseMacroExpansions
          -- Macro types override other types ('Map.union' is left-biased).
          reparseEnv = usedMacroTypes `Map.union` env.knownTypes
      case parser reparseEnv tokens of
        Right a -> do
          modify $ #reparseSuccesses %~ Set.insert declId
          onSuccess a
        Left  e -> do
          modify $ #reparseWarnings %~ ((declId, ParseMacroErrorReparse e) :)
          pure fallback

-- | Dependencies before reparse may point to underlying types. These have to be
-- replaced with their actual dependencies after reparse.
--
-- For example,
--
-- @
-- // c_header.h
-- typedef int A;
-- #define B A
-- void foo(B x);
-- @
--
-- Before reparse, @foo@ directly depends on @A@. After reparse, we know
-- that @foo@ depends on @B@. We have to cut the dependency to @A@ and
-- replace it with the dependency to @B@.
updateDeps :: C.Decl ReparseMacroExpansions -> UseDeclGraph -> UseDeclGraph
updateDeps decl graph =
    UseDeclGraph.insertDepsOfDecl decl $
      UseDeclGraph.deleteDeps [decl.info.id] graph
