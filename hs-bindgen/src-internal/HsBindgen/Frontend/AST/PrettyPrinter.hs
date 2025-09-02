-- | Pretty-print valid C code
module HsBindgen.Frontend.AST.PrettyPrinter (
    showsFunctionType,
    showsVariableType,
    showsType,
    showsFunctionPurity,
  ) where

import Data.Text qualified as Text

import HsBindgen.Frontend.AST.External
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Pretty-printers
-------------------------------------------------------------------------------}

showsFunctionType ::
     HasCallStack
  => ShowS            -- ^ function name
  -> FunctionPurity   -- ^ function purity
  -> [(ShowS, Type)]  -- ^ arguments, names and types
  -> Type             -- ^ return type
  -> ShowS
showsFunctionType n pur args res  =
      showsFunctionPurity pur . showsFunctionPurityWhitespace pur
    . showsType functionDeclarator res
  where
    -- When functions return more complicated types, placing parentheses becomes
    -- tricky. For example, if the function returns a pointer to an array, then
    -- the function and its arguments have to be parenthesised, with the array
    -- brackets placed outside the parantheses. For more information, see the
    -- links below:
    --
    -- <https://en.cppreference.com/w/c/language/function_declaration.html>
    functionDeclarator ::
         CTypePrecedence
      -> ShowS
    functionDeclarator d = showParen (d > arrayPrec) $
        n . showChar ' ' . showParen True signatureArgs

    signatureArgs :: ShowS
    signatureArgs = case args of
        [] -> showString "void"
        p:ps -> foldr1 sep $ fmap showT $ p :| ps
      where
        sep a b = a . showString ", " . b

        showT :: (ShowS, Type) -> ShowS
        showT (i, p) = showsVariableType i p

showsVariableType ::
     HasCallStack
  => ShowS -- ^ variable name
  -> Type
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
showsType ::
     HasCallStack
  => (CTypePrecedence -> ShowS)  -- ^ variable name, or function name + arguments
  -> Type
  -> ShowS
showsType x (TypePrim p)            = C.showsPrimType p . showChar ' ' . x 0
showsType x (TypeStruct np o)       = showString "struct " . showsName np o . showChar ' ' . x 0
showsType x (TypeUnion np o)        = showString "union " . showsName np o . showChar ' ' . x 0
showsType x (TypeEnum np o)         = showString "enum " . showsName np o . showChar ' ' . x 0
showsType x (TypeTypedef ref)       = showsTypedefName ref . showChar ' ' . x 0
showsType x (TypeMacroTypedef np o) = showsName np o . showChar ' ' . x 0
showsType x (TypePointer t)         = showsType (\d -> showParen (d > arrayPrec) $ showString "*" . x (pointerPrec + 1)) t
showsType x (TypeConstArray n t)    = showsType (\_d -> x (arrayPrec + 1) . showChar '[' . shows n . showChar ']') t
showsType x (TypeFun args res)      =
    -- Note: we pass 'ImpureFunction' to 'showsFunctionType' so that no function
    -- attributes are included in the printed string. Function attributes should
    -- not appear inside types, rather only as part of top-level function
    -- declarations.
    showsFunctionType (showParen True (x 0)) ImpureFunction (zipWith named [1..] args) res
  where
    named :: Int -> Type -> (ShowS, Type)
    named i t = (showString "arg" . shows i, t)
showsType x TypeVoid                  = showString "void " . x 0
showsType x (TypeIncompleteArray t)   = showsType (\_d -> x (arrayPrec + 1) . showString "[]") t
showsType x (TypeExtBinding ext)      = showCQualName (extCName ext) . showChar ' ' . x 0
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
showsType x (TypeConst t) = showsType (\_d -> showString "const " . x 0) t

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

-- | Print a single whitespace if the function purity is anything other than
-- 'ImpureFunction'.
showsFunctionPurityWhitespace :: FunctionPurity -> ShowS
showsFunctionPurityWhitespace pur = case pur of
    ImpureFunction -> id
    HaskellPureFunction -> showChar ' '
    CPureFunction -> showChar ' '

showCQualName :: C.QualName -> ShowS
showCQualName = showString . Text.unpack . C.qualNameText

showsName :: NamePair -> NameOrigin -> String -> String
showsName namePair = \case
    NameOriginGenerated{}    -> showString "<anon>"
    NameOriginRenamedFrom nm -> showsCName nm
    _otherwise               -> showsCName (nameC namePair)

showsCName :: Name -> String -> String
showsCName = showString . Text.unpack . getName

showsTypedefName :: TypedefRef -> String -> String
showsTypedefName (TypedefRegular  np)     = showsName  np NameOriginInSource
showsTypedefName (TypedefSquashed nm _ty) = showsCName nm
