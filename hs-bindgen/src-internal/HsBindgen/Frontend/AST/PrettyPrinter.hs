-- | Pretty-print valid C code
module HsBindgen.Frontend.AST.PrettyPrinter (
    showsFunctionType,
    showsType,
    showsFunctionPurity,
  ) where

import Data.Text qualified as Text

import HsBindgen.Frontend.AST.External
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
showsFunctionType n pur args res =
      showsFunctionPurity pur . showsFunctionPurityWhitespace pur
    . showsType n res . showChar ' '
    . showParen True signatureArgs
  where
    signatureArgs :: ShowS
    signatureArgs = case args of
        [] -> showString "void"
        p:ps -> foldr1 sep $ fmap showT $ p :| ps
      where
        sep a b = a . showString ", " . b

        showT :: (ShowS, Type) -> ShowS
        showT (i, p) = showsType i p

-- | Show type in C syntax.
-- Used to generate userland-capi C-code.
--
-- >>> import HsBindgen.Frontend.AST.External (Type (TypePrim))
-- >>> import HsBindgen.Language.C qualified as C
-- >>> showsType (showString "x") (TypePrim C.PrimBool) ""
-- "_Bool x"
--
-- TODO: int (*baz2 (int arg1))[2][3] { ... }
--
--
showsType ::
     HasCallStack
  => ShowS -- ^ variable name
  -> Type
  -> ShowS
showsType x (TypePrim p)            = C.showsPrimType p . showChar ' ' . x
showsType x (TypeStruct np o)       = showString "struct " . showsName np o . showChar ' ' . x
showsType x (TypeUnion np o)        = showString "union " . showsName np o . showChar ' ' . x
showsType x (TypeEnum np o)         = showString "enum " . showsName np o . showChar ' ' . x
showsType x (TypeTypedef ref)       = showsTypedefName ref . showChar ' ' . x
showsType x (TypeMacroTypedef np o) = showsName np o . showChar ' ' . x
showsType x (TypePointer t)         = showsType (showString "*" . x) t
showsType x (TypeConstArray n t)    = showsType (x . showChar '[' . shows n . showChar ']') t
showsType x (TypeFun args res)      =
    -- Note: we pass 'ImpureFunction' to 'showsFunctionType' so that no function
    -- attributes are included in the printed string. Function attributes should
    -- not appear inside types, rather only as part of top-level function
    -- declarations.
    showsFunctionType (showParen True x) ImpureFunction (zipWith named [1..] args) res
  where
    named :: Int -> Type -> (ShowS, Type)
    named i t = (showString "arg" . shows i, t)
showsType x TypeVoid                  = showString "void " . x
showsType x (TypeIncompleteArray t)   = showsType (x . showString "[]") t
showsType x (TypeExtBinding ext)      = showCQualName (extCName ext) . showChar ' ' . x
showsType x (TypeBlock t)             = showsType (showString "^" . x) t

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

showsCName :: C.Name -> String -> String
showsCName = showString . Text.unpack . C.getName

showsTypedefName :: TypedefRef -> String -> String
showsTypedefName (TypedefRegular  np)     = showsName  np NameOriginInSource
showsTypedefName (TypedefSquashed nm _ty) = showsCName nm
