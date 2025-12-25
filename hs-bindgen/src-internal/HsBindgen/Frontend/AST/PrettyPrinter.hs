-- | Pretty-print valid C code
module HsBindgen.Frontend.AST.PrettyPrinter (
    showsFunctionType,
    showsVariableType,
    showsType,
    showsFunctionPurity,
  ) where

import Data.Text qualified as Text

import HsBindgen.Errors
import HsBindgen.Frontend.AST.External
import HsBindgen.Frontend.AST.Type
import HsBindgen.Frontend.Pass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Pretty-printers
-------------------------------------------------------------------------------}

-- | Formats functions with attributes on their own line, parameters on separate
-- lines with additional indentation.
--
showsFunctionType :: forall p.
     (IsPass p, HasCallStack)
  => ShowS              -- ^ function name
  -> FunctionPurity     -- ^ function purity
  -> [(ShowS, Type p)]  -- ^ arguments, names and types
  -> Type p             -- ^ return type
  -> ShowS
showsFunctionType n pur args res  =
      showsFunctionPurity pur
    . showAttributeNewline pur
    . showsType functionDeclarator res
  where
    functionDeclarator ::
         CTypePrecedence
      -> ShowS
    functionDeclarator d = showParen (d > arrayPrec) $
        n . showChar ' ' . showParen True signatureArgs

    signatureArgs :: ShowS
    signatureArgs = case args of
        [] -> showString "void"
        p:ps ->
              showChar '\n'
            . foldr1 sep (fmap showT (p :| ps))
            . showChar '\n'
      where
        sep a b = a . showString ",\n" . b

        showT :: (ShowS, Type p) -> ShowS
        showT (i, p) =
              showString "  "  -- extra 2 spaces for parameters
            . showsVariableType i p

showsVariableType ::
     (IsPass p, HasCallStack)
  => ShowS -- ^ variable name
  -> Type p
  -> ShowS
showsVariableType n ty = showsType variableDeclarator ty
  where
    -- Just the identifier
    variableDeclarator _ = n

-- | Show type in C syntax.
-- Used to generate userland-capi C-code.
--
-- NOTE: it is not unlikely that this function places parentheses incorrectly in
-- edge cases.
--
-- <https://en.cppreference.com/w/c/language/declarations.html>
--
-- === Examples
--
-- >>> import HsBindgen.Frontend.AST.External (Type (..))
-- >>> import HsBindgen.Language.C qualified as C
-- >>> import HsBindgen.Frontend.AST.Internal (FunctionPurity (..))
--
-- A variable @a@ of type boolean:
--
-- >>> showsVariableType (showString "a") (TypePrim C.PrimBool) ""
-- "_Bool a"
--
-- A variable @b@ of type incomplete-array-of-integer:
--
-- >>> :{
--  showsVariableType
--    (showString "b")
--    (TypeIncompleteArray (TypePrim (C.PrimIntegral C.PrimInt C.Signed)))
--    ""
-- :}
-- "signed int b[]"
--
-- A variable @c@ of type pointer-to-3-length-array-of-integer:
--
-- >>> :{
--  showsVariableType
--    (showString "c")
--    (TypePointer (TypeConstArray 3 (TypePrim (C.PrimIntegral C.PrimInt C.Signed))))
--    ""
-- :}
-- "signed int (*c)[3]"
--
-- A variable @d@ of type 3-length-array-of-pointer-to-integer:
--
-- >>> :{
--  showsVariableType
--    (showString "d")
--    (TypeConstArray 3 (TypePointer (TypePrim (C.PrimIntegral C.PrimInt C.Signed))))
--    ""
-- :}
-- "signed int *d[3]"
--
-- A function @foo@ of return type pointer-to-integer:
--
-- >>> :{
--  showsFunctionType
--    (showString "foo")
--    ImpureFunction
--    []
--    (TypePointer (TypePrim (C.PrimIntegral C.PrimInt C.Signed)))
--    ""
-- :}
-- "signed int *foo (void)"
--
-- A function @bar@ of return type pointer-to-2x3-size-array-of-integer:
--
-- >>> :{
--  showsFunctionType
--    (showString "bar")
--    ImpureFunction
--    [(showString "arg1", TypePrim (C.PrimIntegral C.PrimInt C.Signed))]
--    (TypePointer (TypeConstArray 2 (TypeConstArray 3 (TypePrim (C.PrimIntegral C.PrimInt C.Signed)))))
--    ""
-- :}
-- "signed int (*bar (signed int arg1))[2][3]"
--
showsType :: forall p.
     (IsPass p, HasCallStack)
  => (CTypePrecedence -> ShowS)  -- ^ variable name, or function name + arguments
  -> Type p
  -> ShowS
showsType x (TypePrim p)              = C.showsPrimType p . showChar ' ' . x 0
showsType x (TypeRef ref)             = showsId (Proxy @p) ref . showChar ' ' . x 0
showsType x (TypeTypedef typedef)     = showsId (Proxy @p) typedef.ref . showChar ' ' . x 0
showsType x (TypePointers n t)        = showsType (\d -> showParen (d > arrayPrec)
                                      $ foldr (.) id (replicate n (showString "*"))
                                      . x (pointerPrec + 1)) t
showsType x (TypeConstArray n t)      = showsType (\_d -> x (arrayPrec + 1) . showChar '[' . shows n . showChar ']') t
showsType x (TypeFun args res)        =
    -- Note: we pass 'ImpureFunction' to 'showsFunctionType' so that no function
    -- attributes are included in the printed string. Function attributes should
    -- not appear inside types, rather only as part of top-level function
    -- declarations.
    showsFunctionType (showParen True (x 0)) ImpureFunction (zipWith named [1..] args) res
  where
    named :: Int -> Type p -> (ShowS, Type p)
    named i t = (showString "arg" . shows i, t)
showsType x TypeVoid                  = showString "void " . x 0
showsType x (TypeIncompleteArray t)   = showsType (\_d -> x (arrayPrec + 1) . showString "[]") t
showsType x (TypeExtBinding ext)      = showsId (Proxy @p) (extBindingId (Proxy @p) ext) . showChar ' ' . x 0
showsType x (TypeBlock t)             = showsType (\_d -> showString "^" . x 0) t
-- Type qualifiers like @const@ can appear before, and _after_ the type they
-- refer to. For example,
--
-- > const int x;
-- > int const x;
--
-- > const int f();
-- > int const f();
--
-- More involved: A function with a return type being a "constant pointer to
-- constant integer".
--
-- > const int * const f();
-- > int const * const f();
--
-- That is, for pointers, the @const@ qualifier is always written as a suffix!
-- For example, both of the following declarations declare a pointer to a
-- constant integer:
--
-- > int const * f();
-- > int const* f();
--
-- Did you know that stacked @const@ qualifiers are merged by the C parser:
--
-- > const int const * f(); // Parsed as "const int *".
-- > int const const * f(); // Parsed as "const int *".
--
-- It is somewhat difficult to correctly print the @const@ qualifier before
-- primitive types but after pointers. Hence, we consistently print @const@
-- _after_ the type. For example, we print return type "constant pointer to
-- constant int" as follows:
--
-- > int const * const f();
showsType x (TypeQualified TypeQualifierConst t) = showsType (\_d -> showString "const " . x 0) t
showsType x (TypeComplex p) = C.showsPrimType p . showChar ' ' . showString "_Complex " . x 0

-- | The precedence of various constructs in C declarations.
type CTypePrecedence = Int

-- NOTE: picked somewhat arbitrarily to be larger than 'pointerPrec'
arrayPrec :: CTypePrecedence
arrayPrec = 10

-- NOTE: picked somewhat arbitrarily to be smaller than 'arrayPrec'
pointerPrec :: CTypePrecedence
pointerPrec = 5

-- | Show function purity in C syntax.
--
-- Function purity translates to a @const@ or @pure@ function attribute.
--
--
-- >>> import HsBindgen.Frontend.AST.External (FunctionPurity(..))
--
-- >>> showsFunctionPurity ImpureFunction ""
-- ""
--
-- >>> showsFunctionPurity HaskellPureFunction ""
-- "__attribute__ ((const))"
--
-- >>> showsFunctionPurity CPureFunction ""
-- "__attribute__ ((pure))"
showsFunctionPurity :: FunctionPurity -> ShowS
showsFunctionPurity pur = case pur of
    ImpureFunction -> id
    HaskellPureFunction -> withShowsAttribute "const"
    CPureFunction -> withShowsAttribute "pure"
  where
    withShowsAttribute s =
        showString "__attribute__ (("
      . showString s
      . showString "))"

-- | Print a newline after a function attribute, but only if there is an attribute.
--
showAttributeNewline :: FunctionPurity -> ShowS
showAttributeNewline pur = case pur of
    ImpureFunction -> id
    HaskellPureFunction -> showChar '\n'
    CPureFunction -> showChar '\n'

showsId :: IsPass p => Proxy p -> Id p -> ShowS
showsId p declId =
    case idSourceName p declId of
      Just name -> showsDeclName name
      Nothing   -> panicPure $ "Cannot refer to anon decl " ++ show declId

showsDeclName :: C.DeclName -> ShowS
showsDeclName (C.DeclName name kind) = showsNameKind kind . showsText name

showsNameKind :: NameKind -> ShowS
showsNameKind = \case
    NameKindOrdinary    -> id
    NameKindTagged kind ->
      case kind of
       TagKindStruct -> showString "struct "
       TagKindEnum   -> showString "enum "
       TagKindUnion  -> showString "union "

showsText :: Text -> ShowS
showsText = showString . Text.unpack
