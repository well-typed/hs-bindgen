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
    -- * Indirections
  , indirectionsType
  ) where

import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy (..))
import GHC.Stack

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Errors
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.FunSig
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
     FunSig Hs.Type Hs.Type -- ^ Translated in contexts 'FunArg' and 'FunRes'
  -> Hs.Type
funType sig = foldr Hs.Fun (Hs.IO sig.res) sig.args

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
    C.TypeEnum ref ->
      Hs.TypRef
        (Hs.assertNs (Proxy @Hs.NsTypeConstr) ref.name.hsName)
    C.TypeMacro ref ->
      Hs.TypRef
        (Hs.assertNs (Proxy @Hs.NsTypeConstr) ref.name.hsName)
    C.TypeTypedef ref ->
      Hs.TypRef
        (Hs.assertNs (Proxy @Hs.NsTypeConstr) ref.name.hsName)
    C.TypePointers n ty ->
      pointers n (pointee ty) (typeInContext PtrArg ty)
    C.TypeConstArray n ty ->
      Hs.ConstArray n (typeInContext Top ty)
    C.TypeIncompleteArray ty ->
      Hs.IncompleteArray (typeInContext Top ty)
    C.TypeFun sig ->
      funType $ bimap (inContext FunArg) (typeInContext FunRes) sig
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

-- | Haskell type of one or more pointers
--
-- The innermost pointer points to the given 'Pointee'; any others point to
-- pointers.
pointers ::
     Int     -- ^ Number of pointers, at least one
  -> Pointee -- ^ What the innermost pointer points to
  -> Hs.Type -- ^ Haskell type of the pointee
  -> Hs.Type
pointers n p ty =
    foldr ($) (innermost ty) (replicate (n - 1) Hs.Ptr)
  where
    innermost :: Hs.Type -> Hs.Type
    innermost = case p of
      PointeeFunction    -> Hs.FunPtr
      PointeeConstObject -> Hs.PtrConst
      PointeeObject      -> Hs.Ptr

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
  Indirections
-------------------------------------------------------------------------------}

-- | Haskell type of a function type behind indirections
--
-- Translates @C.applyIndirections indirections fun@ at the top level, given the
-- Haskell type of @fun@: a function type, or a reference to one, such as the
-- auxiliary newtype of a typedef. As 'pointee' sees through typedefs and
-- qualifiers, each 'Pointee' follows from the indirections below the pointer:
--
-- * only qualifiers below: 'PointeeFunction';
-- * a @const@ qualifier below, possibly behind arrays: 'PointeeConstObject';
-- * otherwise: 'PointeeObject'.
--
-- The 'TypeContext' does not matter: it affects only the translation of
-- @void@, which cannot occur in the indirections, and the function type is
-- translated independently of its context.
indirectionsType ::
     NonEmpty C.Indirection -- ^ Outermost first
  -> Hs.Type                -- ^ Haskell type of the function type
  -> Hs.Type
indirectionsType indirections fun = go (toList indirections)
  where
    go :: [C.Indirection] -> Hs.Type
    go = \case
      []                        -> fun
      C.Pointers n       : rest -> pointers n (pointeeOf rest) (go rest)
      C.ConstArray n     : rest -> Hs.ConstArray n (go rest)
      C.IncompleteArray  : rest -> Hs.IncompleteArray (go rest)
      C.Block            : rest -> Hs.Block (go rest)
      C.Qual C.QualConst : rest -> go rest

    -- | What a pointer points to, given the indirections below it
    pointeeOf :: [C.Indirection] -> Pointee
    pointeeOf rest
      | all isQual rest     = PointeeFunction
      | constQualified rest = PointeeConstObject
      | otherwise           = PointeeObject

    isQual :: C.Indirection -> Bool
    isQual = \case
      C.Qual _   -> True
      _otherwise -> False

    -- Mirrors 'C.isErasedTypeConstQualified': arrays are @const@-qualified if
    -- their element type is.
    constQualified :: [C.Indirection] -> Bool
    constQualified = \case
      C.Qual C.QualConst : _    -> True
      C.ConstArray _     : rest -> constQualified rest
      C.IncompleteArray  : rest -> constQualified rest
      _otherwise                -> False

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
