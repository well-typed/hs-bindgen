-- | Translate types (use sites)
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Pass.TranslateTypes.Translation qualified as Translation
module HsBindgen.Frontend.Pass.TranslateTypes.Translation (
    -- * Contextual translation
    TypeContext(..)
  , IsPassCompat
  , InContext(..)
    -- ** Helper functions
  , topLevel
  ) where

import Data.Proxy (Proxy (..))
import GHC.Stack

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Errors
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Pass
import HsBindgen.IR.Translation
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Contextual translation
-------------------------------------------------------------------------------}

data TypeContext =
    Top     -- ^ Anything else
  | FunArg  -- ^ Function argument
  | FunRes  -- ^ Function result
  | PtrArg  -- ^ Pointer argument
  deriving stock (Show)

-- | Constraints for compatible passes for C type translation
--
-- Effectively, type translation is supports
-- 'HsBindgen.Frontend.Pass.TranslateTypes.IsPass.TranslateTypes' and later.
type IsPassCompat p = (
    IsPass p
  , Ann "TypeFunArg" p ~ AdjustedFrom p
  , ExtBinding p ~ BindingSpec.ResolvedExtBinding
  , Id p ~ DeclIdPair
  , MacroId p ~ Id p
  , MacroUnderlying p ~ C.Type p
  )

class InContext a where
  inContext :: HasCallStack => TypeContext -> a -> Hs.Type

instance IsPassCompat p => InContext (C.Type p) where
  inContext = typeInContext

instance IsPassCompat p => InContext (C.TypeFunArg p) where
  inContext ctx arg = funArg ctx arg.typ arg.ann

instance IsPassCompat p => InContext (C.FunctionArg p) where
  inContext ctx arg = funArg ctx (cType (Proxy @p) arg.typ) arg.ann

{-------------------------------------------------------------------------------
  Helper functions
-------------------------------------------------------------------------------}

topLevel :: (HasCallStack, IsPassCompat p) => C.Type p -> Hs.Type
topLevel = inContext Top

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

typeInContext ::
     (HasCallStack, IsPassCompat p)
  => TypeContext
  -> C.Type p
  -> Hs.Type
typeInContext ctx = \case
    C.TypePrim p ->
      Hs.PrimType (translateCPrimType p)
    C.TypeComplex p ->
      Hs.ComplexType (translateCPrimType p)
    C.TypeRef ref ->
      Hs.TypRef
        (Hs.assertNs (Proxy @Hs.NsTypeConstr) ref.hsName)
        Nothing
    C.TypeEnum ref ->
      Hs.TypRef
        (Hs.assertNs (Proxy @Hs.NsTypeConstr) ref.name.hsName)
        (Just $ typeInContext ctx ref.underlying)
    C.TypeMacro ref ->
      Hs.TypRef
        (Hs.assertNs (Proxy @Hs.NsTypeConstr) ref.name.hsName)
        (Just $ typeInContext ctx ref.underlying)
    C.TypeTypedef ref ->
      Hs.TypRef
        (Hs.assertNs (Proxy @Hs.NsTypeConstr) ref.name.hsName)
        (Just $ typeInContext ctx ref.underlying)
    C.TypePointers n ty
      -- Use a 'FunPtr' if the type is a function type.  We inspect the
      -- /canonical/ type because we want to see through typedefs and type
      -- qualifiers like @const@.
      | C.isCanonicalTypeFunction ty ->
          foldr
            ($)
            (Hs.FunPtr $ typeInContext PtrArg ty)
            (replicate (n - 1) Hs.Ptr)
      | C.isErasedTypeConstQualified ty ->
          foldr
            ($)
            (Hs.PtrConst $ typeInContext PtrArg ty)
            (replicate (n - 1) Hs.Ptr)
      | otherwise ->
          foldr
            ($)
            (typeInContext PtrArg ty)
            (replicate n Hs.Ptr)
    C.TypeConstArray n ty ->
      Hs.ConstArray n (typeInContext Top ty)
    C.TypeIncompleteArray ty ->
      Hs.IncompleteArray (typeInContext Top ty)
    C.TypeFun args res ->
      foldr
        (\arg acc -> Hs.Fun (inContext FunArg arg) acc)
        (Hs.IO $ typeInContext FunRes res)
        args
    C.TypeVoid ->
      Hs.PrimType (void ctx)
    C.TypeBlock ty ->
      Hs.Block $ typeInContext Top ty
    C.TypeQual C.QualConst ty ->
      typeInContext ctx ty
    C.TypeExtBinding ref ->
      let ext = ref.name
      in  Hs.ExtBinding
            ext.hsName
            ext.cSpec
            ext.hsSpec
            (typeInContext ctx ref.underlying)

-- | Translate a function argument
funArg ::
     (HasCallStack, IsPassCompat p)
  => TypeContext
  -> C.Type p
  -> AdjustedFrom p
  -> Hs.Type
funArg ctx ty = \case
    AdjustedFromArray origTy
      | C.isErasedTypeConstQualified origTy ->
          Hs.PtrConstArrayElem (typeInContext Top origTy)
      | otherwise ->
          Hs.PtrArrayElem (typeInContext Top origTy)
    AdjustedFromFunction _origTy -> typeInContext ctx ty
    NotAdjusted -> typeInContext ctx ty

-- | Translate @void@
--
-- This only makes sense in non-top-level contexts.  (We take special care in
-- macro type parsing to rule out top-level @void@.)
void :: HasCallStack => TypeContext -> Hs.PrimType
void = \case
    FunRes -> Hs.PrimUnit
    PtrArg -> Hs.PrimVoid
    c      -> panicPure ("Unexpected type void in context " ++ show c)
