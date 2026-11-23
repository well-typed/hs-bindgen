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
  , funType
    -- * Pointers
  , Pointee(..)
  , pointee
  ) where

import Data.Proxy (Proxy (..))
import GHC.Stack

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Errors
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
  inContext ctx arg = funArg ctx arg.typ

instance IsPassCompat p => InContext (C.AnonRef p) where
  inContext ctx = typeInContext ctx . C.anonRefType

instance IsPassCompat p => InContext (C.FunctionArg p) where
  inContext ctx arg = inContext ctx (cTypeFunArg (Proxy @p) arg.typ)

instance IsPassCompat p => InContext (C.FunctionRes p) where
  inContext ctx res = typeInContext ctx (cType (Proxy @p) res.typ)

{-------------------------------------------------------------------------------
  Helper functions
-------------------------------------------------------------------------------}

topLevel :: (HasCallStack, IsPassCompat p) => C.Type p -> Hs.Type
topLevel = inContext Top

-- | Haskell type of a function type
funType ::
     [Hs.Type] -- ^ Arguments, translated in context 'FunArg'
  -> Hs.Type   -- ^ Result, translated in context 'FunRes'
  -> Hs.Type
funType args res = foldr Hs.Fun (Hs.IO res) args

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
    C.TypePointers n ty ->
      let innermost = case pointee ty of
            PointeeFunction    -> Hs.FunPtr
            PointeeConstObject -> Hs.PtrConst
            PointeeObject      -> Hs.Ptr
      in  foldr
            ($)
            (innermost $ typeInContext PtrArg ty)
            (replicate (n - 1) Hs.Ptr)
    C.TypeConstArray n ty ->
      Hs.ConstArray n (typeInContext Top ty)
    C.TypeIncompleteArray ty ->
      Hs.IncompleteArray (typeInContext Top ty)
    C.TypeFun args res ->
      funType (map (inContext FunArg) args) (typeInContext FunRes res)
    C.TypeVoid ->
      Hs.PrimType (void ctx)
    C.TypeBlock ty ->
      Hs.Block $ typeInContext Top ty
    C.TypeQual C.QualConst ty ->
      typeInContext ctx ty
    C.TypeExtBinding ref ->
      let ext = ref.name
      in  Hs.ExtBinding ext.hsName ext.cSpec ext.hsSpec

{-------------------------------------------------------------------------------
  Pointers
-------------------------------------------------------------------------------}

-- | What a pointer points to
data Pointee =
    -- | A function; the pointer is a 'Hs.FunPtr'
    PointeeFunction
    -- | A @const@-qualified object; the pointer is a 'Hs.PtrConst'
  | PointeeConstObject
    -- | Any other object; the pointer is a 'Hs.Ptr'
  | PointeeObject
  deriving stock (Show, Eq)

pointee :: MacroUnderlying p ~ C.Type p => C.Type p -> Pointee
pointee ty
    -- We inspect the /canonical/ type to see through typedefs and type
    -- qualifiers like @const@.
    | C.isCanonicalTypeFunction ty    = PointeeFunction
    | C.isErasedTypeConstQualified ty = PointeeConstObject
    | otherwise                       = PointeeObject

-- | Translate @void@
--
-- This only makes sense in non-top-level contexts.  (We take special care in
-- macro type parsing to rule out top-level @void@.)
void :: HasCallStack => TypeContext -> Hs.PrimType
void = \case
    FunRes -> Hs.PrimUnit
    PtrArg -> Hs.PrimVoid
    c      -> panicPure ("Unexpected type void in context " ++ show c)

{-------------------------------------------------------------------------------
  Function argument types
-------------------------------------------------------------------------------}

-- | Translate a function argument
--
-- Some function argument types are adjusted while translating to Haskell types.
-- See 'adjustFunArg'.
funArg ::
     (HasCallStack, IsPassCompat p)
  => TypeContext
  -> C.Type p
  -> Hs.Type
funArg ctx ty = case adjustFunArg ty of
    AdjustArray
      | C.isErasedTypeConstQualified ty ->
          Hs.PtrConstArrayElem (typeInContext Top ty)
      | otherwise ->
          Hs.PtrArrayElem (typeInContext Top ty)
    AdjustFunction -> typeInContext ctx (C.TypePointers 1 ty)
    NoAdjust -> typeInContext ctx ty

-- | Adjust function argument types
--
-- The C reference describes that function parameters of some specific types are
-- adjusted. See:
--
-- <https://en.cppreference.com/w/c/language/function_declaration.html#Explanation>
--
-- We apply some of these adjustments to reduce ambiguity about the
-- representation of function parameter types. These adjustments are applied
-- everywhere that a C type occurs in the AST, even recursively. The adjustments
-- we apply are:
--
-- > * any parameter of function type is adjusted to the corresponding pointer
-- >  type
-- >
-- > * any parameter of array type is adjusted to the corresponding pointer
-- >   type
--
-- Function arguments of function type are changed to *pointer* to function
-- type. For example, the former is adjusted to the latter:
--
-- > int foo (char f (float))
-- > int foo (char (*f)(float))
--
-- Function arguments of array type are changed to pointer to the array element
-- type. For example, the former is adjusted to the latter:
--
-- > int foo (char xs[])
-- > int foo (char * xs)
--
-- === Function results
--
-- As specified by the C reference, a function result type can /not/ be
-- a function type or array type. As such, no need to adjust there.
--
-- https://en.cppreference.com/c/language/function_declaration#Syntax>
--
-- === Macro types
--
-- NOTE: currently type-like macro expressions don't support array or function
-- constructors. If they do in the future, then we might have to recurse into
-- the macro expression.
--
adjustFunArg :: IsPassCompat p => C.Type p -> Adjust
adjustFunArg ty
  | C.isCanonicalTypeArray ty
  = AdjustArray
  | C.isCanonicalTypeFunction ty
  = AdjustFunction
  | otherwise
  = NoAdjust

data Adjust =
    AdjustArray
  | AdjustFunction
  | NoAdjust
  deriving stock (Show, Eq, Ord)
