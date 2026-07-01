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

  -- * Argument and result passing
  , PassBy(..)
  , isPassByAddress
  , PassArgBy
  , passArgBy
  , PassResBy
  , passResBy
  , ToWrapperType(..)
  , ToOrigType(..)
  , ToPrimitiveType(..)
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
topLevel = typeInContext Top

{-------------------------------------------------------------------------------
  Argument and result passing
-------------------------------------------------------------------------------}

-- | Function argument and result classification
--
-- It is either passed by value or by address.
data PassBy byValue byAddress =
      -- | Types passed by value.
      --
      -- Ordinary, "primitive" types which can be handled by Haskell FFI
      -- directly.
      PassByValue byValue
      -- | Types passed by address: union, struct, and complex
      --
      -- These have to be passed by address, because they can not be handled by
      -- the Haskell FFI directly.
    | PassByAddress byAddress
  deriving Show

isPassByAddress :: PassBy byValue byAddress -> Bool
isPassByAddress = \case
    PassByValue{}   -> False
    PassByAddress{} -> True

-- | Function argument classification
type PassArgBy p = PassBy (C.FunctionArg p) (C.Type p)

-- | Determine how a function argument is passed from Haskell to C
passArgBy :: forall p.
     (HasCallStack, IsPassCompat p)
  => C.FunctionArg p
  -> PassArgBy p
passArgBy arg
    -- Heap types
    | C.isCanonicalTypeStruct  ty ||
      C.isCanonicalTypeUnion   ty ||
      C.isCanonicalTypeComplex ty =
        if arg.ann == NotAdjusted
          then PassByAddress ty
          else
            -- Should have been adjusted in the AdjustTypes pass
            panicPure "passArgBy: function argument of type struct/union/complex"

    -- Invalid types
    | C.isCanonicalTypeArray ty =
        -- Should have been adjusted in the AdjustTypes pass
        panicPure "passArgBy: function argument of type array"
    | C.isCanonicalTypeFunction ty =
        -- Should have been adjusted in the AdjustTypes pass
        panicPure "passArgBy: function argument of type function"

    -- Other types
    | otherwise =
        PassByValue arg
  where
    ty :: C.Type p
    ty = cType (Proxy @p) arg.typ

-- | Function result classification
type PassResBy p = PassBy (C.Type p) (C.Type p)

-- | Determine how a function result is passed from C to Haskell
passResBy :: (HasCallStack, IsPassCompat p) => C.Type p -> PassResBy p
passResBy res
    -- Heap types
    | C.isCanonicalTypeStruct  res ||
      C.isCanonicalTypeUnion   res ||
      C.isCanonicalTypeComplex res =
        PassByAddress res

    -- Invalid types
    | C.isCanonicalTypeArray res =
        panicPure "passResBy: array cannot be the result type of a function"
    | C.isCanonicalTypeFunction res =
        panicPure "passResBy: function cannot be the result type of a function"

    -- Other types
    | otherwise =
        PassByValue res

--------------------------------------------------------------------------------

class IsPassCompat p => ToWrapperType a p where
  -- | Recover type used in the C wrapper
  toWrapperType :: a -> C.Type p

instance IsPassCompat p => ToWrapperType (PassArgBy p) p where
  toWrapperType = \case
    PassByValue   arg -> cType (Proxy @p) arg.typ
    PassByAddress ty  -> C.TypePointers 1 ty

instance IsPassCompat p => ToWrapperType (PassResBy p) p where
  toWrapperType = \case
    PassByValue   ty -> ty
    PassByAddress ty -> C.TypePointers 1 ty

--------------------------------------------------------------------------------

class ToOrigType a where
  -- | Recover type used in @restoreOrigSignature@
  toOrigType :: TypeContext -> a -> Hs.Type

instance IsPassCompat p => ToOrigType (PassArgBy p) where
  toOrigType ctx = \case
    PassByValue   arg -> inContext ctx arg
    PassByAddress ty  -> inContext ctx ty

instance IsPassCompat p => ToOrigType (PassResBy p) where
  toOrigType ctx = \case
    PassByValue   ty -> inContext ctx ty
    PassByAddress ty -> inContext ctx ty

--------------------------------------------------------------------------------

class ToPrimitiveType a where
  -- | Recover type used in the foreign import
  toPrimitiveType :: TypeContext -> a -> Hs.Type

instance IsPassCompat p => ToPrimitiveType (PassArgBy p) where
  toPrimitiveType ctx = \case
    PassByValue   arg -> inContext ctx arg
    PassByAddress ty  -> inContext ctx (C.TypePointers 1 ty)

instance IsPassCompat p => ToPrimitiveType (PassResBy p) where
  toPrimitiveType ctx = \case
    PassByValue   ty -> inContext ctx ty
    PassByAddress ty -> inContext ctx (C.TypePointers 1 ty)

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
