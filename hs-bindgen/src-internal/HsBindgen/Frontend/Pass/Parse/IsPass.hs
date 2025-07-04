module HsBindgen.Frontend.Pass.Parse.IsPass (
    Parse
    -- * Macros
  , UnparsedMacro(..)
  , ReparseInfo(..)
  , getUnparsedMacro
    -- * Trace messages
  , Msg(..)
  ) where

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
  type Id         Parse = DeclId
  type FieldName  Parse = CName
  type TypedefRef Parse = CName
  type MacroBody  Parse = UnparsedMacro
  type ExtBinding Parse = Void
  type Ann ix     Parse = AnnParse ix
  data Msg        Parse =
      -- | We skipped over a declaration
      Skipped Predicate.SkipReason

      -- | Unsupported type
      --
      -- Since types don't necessarily have associated source locations, we
      -- instead record information about the enclosing declaration.
    | UnsupportedType (C.DeclInfo Parse) ParseTypeException

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
      -- It's not entirely clear if C23/WG14-N3037 affects this or not; @gcc@
      -- warns about both declarations even with @-std=c2x@.
      --
      -- For our purposes, only the anonymous case is really problematic (we have
      -- no way of assigning a name to the struct). Since it is relatively clear
      -- that the anonymous version is anyway unusable (callers would have no way
      -- of constructing any values), we rule them out.
    | UnexpectedAnonInSignature (C.DeclInfo Parse)
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
     MonadIO m
  => CXTranslationUnit -> CXCursor -> m UnparsedMacro
getUnparsedMacro unit curr = do
    range  <- HighLevel.clang_getCursorExtent curr
    tokens <- HighLevel.clang_tokenize unit (multiLocExpansion <$> range)
    return $ UnparsedMacro tokens

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

instance PrettyForTrace (C.DeclInfo Parse) where
  prettyForTrace C.DeclInfo{declId, declLoc} =
      case declId of
        DeclNamed name -> hcat [
            prettyForTrace name
          , " at "
          , showToCtxDoc declLoc
          ]
        DeclAnon anonId ->
            -- No need to repeat the source location in this case
            prettyForTrace anonId

instance PrettyForTrace (Msg Parse) where
  prettyForTrace = \case
      Skipped reason ->
          prettyForTrace reason
      UnsupportedType info err -> noBindingsGenerated info $
          prettyForTrace err
      UnsupportedImplicitFields info -> noBindingsGenerated info $
          "unsupported implicit fields"
      UnexpectedAnonInSignature info -> noBindingsGenerated info $
          "unexpected anonymous declaration in function signature"
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
      Skipped reason              -> getDefaultLogLevel reason
      UnsupportedType _ctxt err   -> getDefaultLogLevel err
      UnsupportedImplicitFields{} -> Warning
      UnexpectedAnonInSignature{} -> Warning

instance HasSource (Msg Parse) where
    getSource = const HsBindgen
