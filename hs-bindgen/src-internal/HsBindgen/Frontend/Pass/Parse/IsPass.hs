module HsBindgen.Frontend.Pass.Parse.IsPass (
    Parse
    -- * Macros
  , UnparsedMacro(..)
  , ReparseInfo(..)
  , getUnparsedMacro
    -- * Tracing
  , ParseMsg(..)
  ) where

import Data.List qualified as List

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Frontend.AST.Internal (ValidPass)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.NonSelectedDecls (NonSelectedDecls)
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Type.DeclId
import HsBindgen.Frontend.Pass.Parse.Type.Monad (ParseTypeException)
import HsBindgen.Imports
import HsBindgen.Language.C
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint qualified as PP

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Parse :: Pass
data Parse a deriving anyclass ValidPass

type family AnnParse (ix :: Symbol) :: Star where
  AnnParse "TranslationUnit" = NonSelectedDecls
  AnnParse "StructField"     = ReparseInfo
  AnnParse "UnionField"      = ReparseInfo
  AnnParse "Typedef"         = ReparseInfo
  AnnParse "Function"        = ReparseInfo
  AnnParse _                 = NoAnn

instance IsPass Parse where
  type Id         Parse = DeclId
  type FieldName  Parse = CName
  type TypedefRef Parse = CName
  type MacroBody  Parse = UnparsedMacro
  type ExtBinding Parse = Void
  type Ann ix     Parse = AnnParse ix

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

newtype UnparsedMacro = UnparsedMacro {
      unparsedTokens :: [Token TokenSpelling]
    }
  deriving stock (Show, Eq)

data ReparseInfo =
    -- | We need to reparse this declaration (to deal with macros)
    --
    -- NOTE: We do not use this for macros /themselves/ (see 'UnparsedMacro').
    ReparseNeeded [Token TokenSpelling]

    -- | This declaration does not use macros, so no need to reparse
  | ReparseNotNeeded
  deriving stock (Show, Eq)

getUnparsedMacro ::
     MonadIO m
  => CXTranslationUnit -> CXCursor -> m UnparsedMacro
getUnparsedMacro unit curr = do
    range  <- HighLevel.clang_getCursorExtent curr
    tokens <- HighLevel.clang_tokenize unit (multiLocExpansion <$> range)
    return $ UnparsedMacro tokens

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data ParseMsg =
    -- | We skipped over a declaration
    Skipped Predicate.SkipReason

    -- | Unsupported type
    --
    -- Since types don't necessarily have associated source locations, we
    -- instead record information about the enclosing declaration.
  | UnsupportedType {
        unsupportedTypeContext   :: C.DeclInfo Parse
      , unsupportedTypeException :: ParseTypeException
      }

    -- | Struct with implicit fields
  | UnsupportedImplicitFields (C.DeclInfo Parse)

    -- | Function signature with nested declarations
    --
    -- Examples:
    --
    -- > void f(struct named { int x; int y; } arg);
    -- > void g(struct { int x; int y; } arg);
    --
    -- Somewhat surprisingly, @clang@ warns about the first
    --
    -- > warning: declaration of 'struct named' will not be visible outside of
    -- > this function
    --
    -- but accepts the second (anonymous struct); @gcc@ warns in both cases:
    --
    -- > {‘struct named’, anonymous struct} declared inside parameter list will
    -- > not be visible outside of this definition or declaration
    --
    -- as does the C compiler used by vscode, which in both cases says
    --
    -- > type definition not allowed
    --
    -- Since it seems that such declarations are therefore anyway unusable, we
    -- don't support them either. This avoids having to think about how to
    -- /name/ such anonymous structs; we should perhaps use the function
    -- parameters, but function signatures don't always list them; the name of
    -- the function may not suffice, because there might be more than one; for
    -- example in
    --
    -- > #define point struct { int x; int y; }
    -- > void h(point p1, point p2);
  | UnsupportedDeclsInSignature {
        unsupportedDeclsInSignatureFun   :: C.DeclInfo Parse
      , unsupportedDeclsInSignatureDecls :: [DeclId]
      }
  deriving stock (Show, Eq)

instance PrettyForTrace ParseMsg where
  prettyForTrace = \case
      Skipped reason ->
          prettyForTrace reason
      UnsupportedType {..} -> PP.hcat [
          "Encountered unsupported type while parsing "
        , prettyForTrace unsupportedTypeContext
        , ": "
        , prettyForTrace unsupportedTypeException
        ]
      UnsupportedImplicitFields info -> PP.hsep [
          "Unsupported implicit fields in"
        , prettyForTrace info
        ]
      UnsupportedDeclsInSignature{..} -> PP.hsep [
          "Unsupported nested declarations in"
        , prettyForTrace unsupportedDeclsInSignatureFun
        , "of"
        , PP.hcat $ List.intersperse ", " $
            map prettyForTrace unsupportedDeclsInSignatureDecls
        ]

-- | Unsupported features are warnings, because we skip over them
instance HasDefaultLogLevel ParseMsg where
  getDefaultLogLevel = \case
      Skipped reason                -> getDefaultLogLevel reason
      UnsupportedType _ctxt err     -> getDefaultLogLevel err
      UnsupportedImplicitFields{}   -> Warning
      UnsupportedDeclsInSignature{} -> Warning

instance HasSource ParseMsg where
    getSource = const HsBindgen
