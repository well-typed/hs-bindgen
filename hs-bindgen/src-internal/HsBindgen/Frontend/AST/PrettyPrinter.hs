-- | Pretty-print valid C code
module HsBindgen.Frontend.AST.PrettyPrinter (
    showsFunctionType,
    showsType,
  ) where

import Data.Text qualified as Text

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.AST.External
import HsBindgen.Imports
import HsBindgen.Language.C

{-------------------------------------------------------------------------------
  Pretty-printers
-------------------------------------------------------------------------------}

showsFunctionType ::
     HasCallStack
  => ShowS            -- ^ function name
  -> [(ShowS, Type)]  -- ^ arguments, names and types
  -> Type             -- ^ return type
  -> ShowS
showsFunctionType n args res =
    showsType n res . showChar ' ' . showParen True signatureArgs
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
-- >>> import HsBindgen.Language.C (PrimType (PrimBool))
-- >>> showsType (showString "x") (TypePrim PrimBool) ""
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
showsType x (TypePrim p)            = showsPrimType p . showChar ' ' . x
showsType x (TypeStruct name)       = showString "struct " . showsName name . showChar ' ' . x
showsType x (TypeUnion name)        = showString "union " . showsName name . showChar ' ' . x
showsType x (TypeEnum name)         = showString "enum " . showsName name . showChar ' ' . x
showsType x (TypeTypedef ref)       = showsTypedefName ref . showChar ' ' . x
showsType x (TypePointer t)         = showsType (showString "*" . x) t
showsType x (TypeConstArray n t)    = showsType (x . showChar '[' . shows n . showChar ']') t
showsType x (TypeFun args res)      = showsFunctionType (showParen True x) (zipWith named [1..] args) res where
  named :: Int -> Type -> (ShowS, Type)
  named i t = (showString "arg" . shows i, t)
showsType x TypeVoid                = showString "void " . x
showsType x (TypeIncompleteArray t) = showsType (x . showString "[]") t
showsType x (TypeExtBinding c _ _)  = showCSpelling c . showChar ' ' . x

showCSpelling :: BindingSpec.CSpelling -> ShowS
showCSpelling = showString . Text.unpack . BindingSpec.getCSpelling

-- TODO: Currently 'NamePair' contains a 'CName' which /we/ constructed.
-- We might want to extend 'CName' with an additional field which tells us
-- whether the original was anonymous or not, and show these as @<anon>@.
showsName :: NamePair -> String -> String
showsName = showsCName . nameC

showsCName :: CName -> String -> String
showsCName = showString . Text.unpack . getCName

showsTypedefName :: TypedefRef -> String -> String
showsTypedefName (TypedefRegular  nm    ) = showsName  nm
showsTypedefName (TypedefSquashed nm _ty) = showsCName nm

