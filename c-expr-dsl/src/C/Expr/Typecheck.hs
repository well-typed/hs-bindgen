{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >=908
{-# LANGUAGE TypeAbstractions #-}
#endif

module C.Expr.Typecheck (
    tcMacros
  , TypeSource(..)
  , CheckedMacroTypeExpr(..)
  , CheckedMacroValueExpr(..)
  , MacroTcResult(..)

    -- * Errors
  , MacroTcError(..)
  , pprMacroTcError
  ) where

import Control.Monad.Except (Except, runExcept)
import Data.Foldable qualified as Foldable
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Type.Equality ((:~:) (..))
import Data.Type.Nat qualified as Nat
import Data.Vec.Lazy (Vec)
import Data.Vec.Lazy qualified as Vec
import GHC.Generics

import C.Expr.Syntax
import C.Expr.Typecheck.Expr
import C.Expr.Typecheck.Interface.Type qualified as T
import C.Expr.Typecheck.Interface.Value qualified as V
import C.Expr.Typecheck.Type
import C.Expr.Util.Panic (panicPure)

{-------------------------------------------------------------------------------
  Typechecking macros: public entry point
-------------------------------------------------------------------------------}

-- | Source of a type-position name: either a @typedef@ or a type-like macro.
--
-- Passed to the @injectType@ callback of 'tcMacros' so that the caller can
-- distinguish the two without maintaining its own state.
data TypeSource =
    -- | A C @typedef@ name (an ordinary type alias).
    TypeSourceTypedef
    -- | A macro that expands to a type expression (e.g. @\#define T int@).
  | TypeSourceMacroType
  deriving stock (Eq, Ord, Show, Generic)

-- | The macro is a C type expression (e.g., @#define FOO int@).
data CheckedMacroTypeExpr var = CheckedMacroTypeExpr{
      macroTypeBody :: T.Expr var
    , macroTypeType :: Quant (FunValue, Type Ty)
    }
  deriving stock (Eq, Show, Generic, Functor)

-- | The macro is a value expression (e.g., @#define BAR 1@).
data CheckedMacroValueExpr var = forall ctx. CheckedMacroValueExpr{
      macroValueParams :: Vec ctx Name
    , macroValueBody   :: V.Expr ctx var
      -- TODO <https://github.com/well-typed/hs-bindgen/issues/1950>
      --
      -- We should not require 'FunValue's for value-like expressions.
    , macroValueType   :: Quant (FunValue, Type Ty)
    }
instance Eq var => Eq (CheckedMacroValueExpr var) where
  (CheckedMacroValueExpr @_ @c1 p1 b1 t1) == (CheckedMacroValueExpr @_ @c2 p2 b2 t2) =
    t1 == t2 && (
      Vec.withDict p1 $ Vec.withDict p2 $
        case Nat.eqNat @c1 @c2 of
          Just Refl -> p1 == p2 && b1 == b2
          Nothing   -> False
    )
deriving stock instance Show var => Show (CheckedMacroValueExpr var)

-- | The result of typechecking a single macro.
data MacroTcResult err var =
    MacroTcTypeExpr    (CheckedMacroTypeExpr  var)
  | MacroTcValueExpr   (CheckedMacroValueExpr var)
  -- | The caller's @injectTaggedType@ callback signalled a domain-level failure.
  | MacroTcInjectError err
  -- | The @c-expr-dsl@ typechecker rejected the macro ('MacroTcCheckError').
  | MacroTcError       MacroTcError

deriving stock instance (Show err, Show var) => Show (MacroTcResult err var)
deriving stock instance (Eq   err, Eq   var) => Eq   (MacroTcResult err var)

-- | Batch-typecheck a sequence of macros
--
-- The macros are processed in order. Each successful macro is added to the
-- internal 'TypeEnv' so that subsequent macros can reference it. A macro that
-- fails to typecheck is /not/ added to the environment; later macros that
-- reference it will fail with an unbound-variable error.
--
-- The three callbacks @injectType@, @injectTaggedType@, and @injectValue@ are
-- invoked while traversing each macro's typechecked body to translate
-- referenced names to the caller's @var@ type. The @injectType@ callback
-- receives a 'TypeSource' so the caller can distinguish @typedef@ references
-- from references to previously-defined type-like macros without maintaining
-- its own state.
--
-- For @typedef@s and values, the typechecker checks the scope. However, the
-- typechecker considers tagged types to be constants, and so it is up to the
-- caller to verify that the constants exist (i.e., the injection callback may
-- fail using 'Except' so the caller can signal a per-macro failure; for
-- example, a referenced @struct@\/@union@\/@enum@ has no resolvable
-- definition). Such a failure is scoped to its macro: it becomes the macro's
-- 'MacroTcInjectError'.
tcMacros ::
     forall var err.
     Set Name                               -- ^ known typedef names
  -> (TypeSource -> Name -> var)            -- ^ inject type name
  -> (Name               -> var)            -- ^ inject value name
  -> (TagKind    -> Name -> Except err var) -- ^ inject tagged type name
  -> [Macro]
  -> Map Name (MacroTcResult err var)
tcMacros typedefs injectType injectValue injectTaggedType macros =
    let (_, _, tcRs) = Foldable.foldl' step initEnv macros
    in  tcRs
  where
    initTypeSources :: Map Name TypeSource
    initTypeSources = Map.fromSet (const TypeSourceTypedef) typedefs

    initEnv :: (TypeEnv, Map Name TypeSource, Map Name a)
    initEnv = (buildTypedefEnv typedefs, initTypeSources, Map.empty)

    step ::
         (TypeEnv, Map Name TypeSource, Map Name (MacroTcResult err var))
      -> Macro
      -> (TypeEnv, Map Name TypeSource, Map Name (MacroTcResult err var))
    step (env, typeSources, acc) (Macro _loc name params body) =
      let injectTypeWithSource :: Name -> var
          injectTypeWithSource nm =
            injectType (lookupTypeSource typeSources nm) nm
          eRes =
            runExcept $
              tcMacroOne env
                injectTypeWithSource
                injectValue
                injectTaggedType
                name params body
          result :: MacroTcResult err var
          result = case eRes of
            Left injE -> MacroTcInjectError injE
            Right r   -> r
          (env', typeSources') = case result of
            MacroTcTypeExpr cmt ->
              ( Map.insert name (macroTypeType  cmt) env
              , Map.insert name TypeSourceMacroType typeSources )
            MacroTcValueExpr cmv ->
              ( Map.insert name (macroValueType cmv) env
              , typeSources )
            MacroTcInjectError _ ->
              (env, typeSources)
            MacroTcError       _ ->
              (env, typeSources)
      in  (env', typeSources', Map.insert name result acc)

    -- Resolve the 'TypeSource' of a type-position name. A missing entry means
    -- the name is value-like or unknown, indicating a bug in the type checker.
    lookupTypeSource :: Map Name TypeSource -> Name -> TypeSource
    lookupTypeSource typeSources nm = case Map.lookup nm typeSources of
      Just k  -> k
      Nothing -> panicPure $ "tcMacros: unavailable type source: " <> show nm

{-------------------------------------------------------------------------------
  Internal: typecheck a single macro against a given 'TypeEnv'.
-------------------------------------------------------------------------------}

-- | Typecheck a single macro against a given 'TypeEnv'.
--
-- The result is a 'MacroTcResult' that never carries 'MacroTcInjectError':
-- inject failures are produced by 'tcMacros' from effects in the caller's
-- monad, not by 'tcMacroOne' itself.
tcMacroOne ::
     forall m ctx var err.
     Applicative m
  => TypeEnv
  -> (Name            ->   var)
  -> (Name            ->   var)
  -> (TagKind -> Name -> m var)
  -> Name
  -> Vec ctx Name
  -> Expr ctx Ps
  -> m (MacroTcResult err var)
tcMacroOne tyEnv injectType injectValue injectTaggedType name params expr =
    case tcExpr tyEnv name params expr of
      Left  err -> pure $ MacroTcError err
      Right res -> classify res
  where
    classify ::
         (Type Ty, Quant (FunValue, Type Ty))
      -> m (MacroTcResult err var)
    classify = \case
      (MacroTypeTy, quant)
        | not (Vec.null params) ->
          pure $ MacroTcError $
            TcUnsupportedTypeWithLocalParameters name (Vec.toList params)
        | otherwise ->
          (\texpr ->
            if isIncompleteType texpr then
              MacroTcError $ TcIncompleteTypeMacro name
            else
              MacroTcTypeExpr $ CheckedMacroTypeExpr texpr quant
          ) <$> T.fromExpr injectType injectTaggedType expr
      (_, quant) ->
        pure $
          (\vexpr -> MacroTcValueExpr $
            CheckedMacroValueExpr params vexpr quant) $
              V.fromExpr injectValue expr

    -- | An incomplete type at the top level of a type-like macro: 'void' or
    -- 'const'-wrapped 'void'. Pointer indirection makes the type complete, so
    -- 'void *' (and 'const void *') are not flagged.
    isIncompleteType :: T.Expr var -> Bool
    isIncompleteType = \case
        T.TypeLit TypeVoid -> True
        T.App T.Const e    -> isIncompleteType e
        _                  -> False
