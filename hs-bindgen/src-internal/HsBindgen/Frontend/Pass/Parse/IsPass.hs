module HsBindgen.Frontend.Pass.Parse.IsPass (
    Parse
    -- * Macros
  , UnparsedMacro(..)
  , ReparseInfo(..)
  , getUnparsedMacro
    -- * Trace messages
  , Msg(..)
  ) where

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import HsBindgen.Frontend.AST.Internal (ValidPass)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.NonSelectedDecls (NonSelectedDecls)
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Type.Monad (ParseTypeException)
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint

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
  type Id         Parse = PrelimDeclId
  type FieldName  Parse = C.Name
  type TypedefRef Parse = C.Name
  type MacroBody  Parse = UnparsedMacro
  type ExtBinding Parse = Void
  type Ann ix     Parse = AnnParse ix

  -- | Parse messages
  --
  -- We distinguish between \"unsupported\", which refers to C features that
  -- one could reasonably expect to be supported eventually, and \"unexpected\",
  -- for strange C input.
  data Msg Parse =
      -- | We skipped over a declaration
      Skipped (C.DeclInfo Parse)

      -- | Unsupported type
      --
      -- Since types don't necessarily have associated source locations, we
      -- instead record information about the enclosing declaration.
    | UnsupportedType (C.DeclInfo Parse) ParseTypeException

      -- | Struct with implicit fields
    | UnsupportedImplicitFields (C.DeclInfo Parse)

      -- | Unexpected anonymous declaration inside function signature
      --
      -- Consider:
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
      -- It's not entirely clear if C23/WG14-N3037 affects this or not; @gcc@
      -- warns about both declarations even with @-std=c2x@.
      --
      -- For our purposes, only the anonymous case is really problematic (we have
      -- no way of assigning a name to the struct). Since it is relatively clear
      -- that the anonymous version is anyway unusable (callers would have no way
      -- of constructing any values), we rule them out.
    | UnexpectedAnonInSignature (C.DeclInfo Parse)

      -- | Unexpected anonymous declaration inside @extern@
      --
      -- Something like
      --
      -- > extern struct { .. } config;
      --
      -- does not make sense: this declares the existence of some externally
      -- defined global variable, but it is impossible to actually define said
      -- global variable; an attempt such as
      --
      -- > #include "config.h"
      -- > struct { .. } config = ..
      --
      -- will result in an error: "conflicting types for 'config'".
      --
      -- The /header/ however by itself will not result in a @clang@ warning, so
      -- we detect the siutation and warn the user in @hs-bindgen@.
      --
      -- (As of C23, the situation is different for /named/ structs: multiple
      -- uses of a struct with the same name are considered compatible as of
      -- WG14-N3037.)
    | UnexpectedAnonInExtern (C.DeclInfo Parse)

      -- | Thread local variables
      --
      -- <https://github.com/well-typed/hs-bindgen/issues/828>
    | UnsupportedTLS (C.DeclInfo Parse)

      -- | Variable declaration
    | UnknownStorageClass (C.DeclInfo Parse) (SimpleEnum CX_StorageClass)

      -- | Global variables without `extern` or `static`
      --
      -- Such definitions can lead to duplicate symbols (linker errors) if they
      -- are included more than once (see manual section on globals for details).
    | PotentialDuplicateGlobal (C.DeclInfo Parse)

      -- | Constants are not yet supported
      --
      -- <https://github.com/well-typed/hs-bindgen/issues/41>
    | UnsupportedConst (C.DeclInfo Parse)
    deriving stock (Show, Eq)

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
     (MonadIO m, HasCallStack)
  => CXTranslationUnit -> CXCursor -> m UnparsedMacro
getUnparsedMacro unit curr = do
    range  <- HighLevel.clang_getCursorExtent curr
    tokens <- HighLevel.clang_tokenize unit (multiLocExpansion <$> range)
    return $ UnparsedMacro tokens

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

instance PrettyForTrace (Msg Parse) where
  prettyForTrace = \case
      Skipped info ->
          prettyForTrace info >< " not selected"
      UnsupportedType info err -> noBindingsGenerated info $
          prettyForTrace err
      UnsupportedImplicitFields info -> noBindingsGenerated info $
          "unsupported implicit fields"
      UnexpectedAnonInSignature info -> noBindingsGenerated info $
          "unexpected anonymous declaration in function signature"
      UnexpectedAnonInExtern info -> noBindingsGenerated info $
          "unexpected anonymous declaration in global variable"
      UnsupportedTLS info -> noBindingsGenerated info $
          "unsupported thread-local variable"
      UnknownStorageClass info storage -> noBindingsGenerated info $ hsep [
          "unsupported storage class"
        , showToCtxDoc storage
        ]
      PotentialDuplicateGlobal info -> hcat [
          "Bindings generated for "
        , prettyForTrace info
        , " may result in duplicate symbols; "
        , "consider using 'static' or 'extern'"
        ]
      UnsupportedConst info -> noBindingsGenerated info $
          "constants not yet supported (#41)"
    where
      noBindingsGenerated :: C.DeclInfo Parse -> CtxDoc -> CtxDoc
      noBindingsGenerated info reason = hcat [
            "No bindings generated for "
          , prettyForTrace info
          , ": "
          , reason
          ]

-- | Unsupported features are warnings, because we skip over them
instance HasDefaultLogLevel (Msg Parse) where
  getDefaultLogLevel = \case
      Skipped{}                   -> Info
      UnsupportedType _ctxt err   -> getDefaultLogLevel err
      UnsupportedImplicitFields{} -> Warning
      UnexpectedAnonInSignature{} -> Warning
      UnexpectedAnonInExtern{}    -> Warning
      UnsupportedTLS{}            -> Warning
      UnknownStorageClass{}       -> Warning
      PotentialDuplicateGlobal{}  -> Notice
      UnsupportedConst{}          -> Warning

instance HasSource (Msg Parse) where
    getSource = const HsBindgen
