module HsBindgen.Frontend.Pass.Parse.IsPass (
    Parse
  , ParseDeclMeta(..)
  , emptyParseDeclMeta
  , ParseOmissionReason(..)
  , DeclStatus(..)
  , ParseStatusValue(..)
  , ParseParseStatus(..)
  , addParseParseStatus
    -- * Macros
  , UnparsedMacro(..)
  , ReparseInfo(..)
  , getUnparsedMacro
    -- * Trace messages
  , ParseTypeExceptionContext(..)
  , UnattachedParseMsg(..)
  , DelayedParseMsg(..)
  ) where

import Text.SimplePrettyPrint (CtxDoc, (<+>))
import Text.SimplePrettyPrint qualified as PP

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming (NameKind, QualPrelimDeclId)
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Type.Monad
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Parse :: Pass
data Parse a deriving anyclass C.ValidPass

type family AnnParse (ix :: Symbol) :: Star where
  AnnParse "TranslationUnit" = ParseDeclMeta
  AnnParse "StructField"     = ReparseInfo
  AnnParse "UnionField"      = ReparseInfo
  AnnParse "Typedef"         = ReparseInfo
  AnnParse "Function"        = ReparseInfo
  AnnParse _                 = NoAnn

instance IsPass Parse where
  type Id           Parse = C.PrelimDeclId
  type FieldName    Parse = C.Name
  type ArgumentName Parse = Maybe C.Name
  type TypedefRef   Parse = C.Name
  type MacroBody    Parse = UnparsedMacro
  type ExtBinding   Parse = Void
  type Ann ix       Parse = AnnParse ix
  type Msg          Parse = UnattachedParseMsg

{-------------------------------------------------------------------------------
  Information about the declarations
-------------------------------------------------------------------------------}

data ParseDeclMeta = ParseDeclMeta {
      parseStatus :: ParseParseStatus
    }
  deriving stock (Show)

emptyParseDeclMeta :: ParseDeclMeta
emptyParseDeclMeta = ParseDeclMeta {
      parseStatus = mempty
    }

-- | Why did we not attempt to parse a declaration?
data ParseOmissionReason =
    -- | We do not parse builtin declarations.
    OmittedBuiltin

    -- | We unexpectedly excluded a declaration because it is reported
    -- "unavailable".
  | DeclarationUnavailable

    -- | Declarations that do not match the parse predicate.
    --
    -- For example, we may provide external bindings for skipped declarations.
    -- We do /not/ support external bindings for /anonymous/ non-parsed
    -- declarations; /if/ you want to provide an external binding for some local
    -- type, for example
    --
    -- > struct rect {
    -- >   struct { int x; int y; } bottomleft;
    -- >   struct { int x; int y; } topright;
    -- > };
    --
    -- then you need to make sure that you /traverse/ @rect@, so that the
    -- @NameAnon@ pass can do its work.
  | ParsePredicateNotMatched
  deriving stock (Show, Eq, Ord)

instance PrettyForTrace ParseOmissionReason where
  prettyForTrace = \case
    OmittedBuiltin           -> "Builtin declaration"
    DeclarationUnavailable   -> "Declaration is 'unavailable' on this platform"
    ParsePredicateNotMatched -> "Parse predicate did not match"

-- | It is important to keep track of the parse and reification status of
-- declarations.
data DeclStatus =
    -- | We did not attempt to parse the declaration.
    ParseNotAttempted ParseOmissionReason

    -- | Happy case ;-). Declarations that match the parse predicate and that we
    -- succeed to parse and reify.
  | ParseSucceeded [DelayedParseMsg]

    -- | Declarations that match the parse predicate and that we fail to parse
    -- and reify.
    --
    -- The generation of bindings will fail if we happen to select failed
    -- bindings.
  | ParseFailed (NonEmpty DelayedParseMsg)
  deriving stock (Show, Eq, Ord)

-- | We collect information about the parse status of declarations. We need this
-- information when selecting declarations (Does the user want to select this
-- declaration, although we have failed to parse it?).
data ParseStatusValue = ParseStatusValue {
      psvSingleLoc    :: SingleLoc
    , psvAvailability :: C.Availability
    , psvDeclStatus   :: DeclStatus
    }
  deriving stock (Show, Eq, Generic)

-- | Parse status of declarations
--
-- Parse can
-- - be not attempted (e.g., because the parse predicate does not match)
-- - succeed (possibly with delayed parse messages), or
-- - fail (with delayed parse messages).
newtype ParseParseStatus = ParseParseStatus {
      unParseParseStatus :: [(QualPrelimDeclId, ParseStatusValue)]
    }
  deriving stock (Show, Eq, Generic)
  deriving newtype (Semigroup, Monoid)

addParseParseStatus :: (QualPrelimDeclId, ParseStatusValue) -> ParseParseStatus -> ParseParseStatus
addParseParseStatus x = ParseParseStatus . (x :) . unParseParseStatus

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

newtype UnparsedMacro = UnparsedMacro {
      unparsedTokens :: [Token TokenSpelling]
    }
  deriving stock (Show, Eq, Ord)

data ReparseInfo =
    -- | We need to reparse this declaration (to deal with macros)
    --
    -- NOTE: We do not use this for macros /themselves/ (see 'UnparsedMacro').
    ReparseNeeded [Token TokenSpelling]

    -- | This declaration does not use macros, so no need to reparse
  | ReparseNotNeeded
  deriving stock (Show, Eq, Ord)

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

data ParseTypeExceptionContext = ParseTypeExceptionContext {
      contextInfo     :: C.DeclInfo Parse
    , contextNameKind :: NameKind
    }
  deriving stock (Show)

instance PrettyForTrace ParseTypeExceptionContext where
  prettyForTrace (ParseTypeExceptionContext info kind) =
    prettyForTrace info <+> ", name kind: " <+> prettyForTrace kind

-- | Parse messages not attached to a declaration
--
-- If we can not attach messages to declarations, we emit them directly while
-- parsing.
data UnattachedParseMsg =
    -- | Declaration availability can not be determined.
    --
    -- That is 'Clang.LowLevel.Core.clang_getCursorAvailability' does not
    -- provide a valid 'Clang.LowLevel.Core.CXAvailabilityKind'.
    ParseUnknownCursorAvailability (C.DeclInfo Parse) (SimpleEnum CXAvailabilityKind)
  deriving stock (Show)

instance PrettyForTrace UnattachedParseMsg where
  prettyForTrace = \case
      ParseUnknownCursorAvailability info simpleKind ->
        withInfo info $
          "unknown declaration availability:" <+> PP.showToCtxDoc simpleKind
    where withInfo info doc = PP.hsep [
              prettyForTrace info
            , doc
            ]

instance IsTrace Level UnattachedParseMsg where
  getDefaultLogLevel = \case
      ParseUnknownCursorAvailability{} -> Notice
  getSource  = const HsBindgen
  getTraceId = const "parse-unattached"

-- | Delayed parse messages
--
-- We emit these parse messages only when we attempt to select the attached
-- declaration.
--
-- We distinguish between \"unsupported\", which refers to C features that one
-- could reasonably expect to be supported eventually, and \"unexpected\", for
-- strange C input.
data DelayedParseMsg =
    -- | Unsupported type
    --
    -- Since types don't necessarily have associated source locations, we
    -- instead record information about the enclosing declaration.
    ParseUnsupportedType ParseTypeException

    -- | Struct with implicit fields
  | ParseUnsupportedImplicitFields

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
  | ParseUnexpectedAnonInSignature

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
    -- (As of C23, the situation is different for /named/ structs: multiple uses
    -- of a struct with the same name are considered compatible as of
    -- WG14-N3037.)
  | ParseUnexpectedAnonInExtern

    -- | Thread local variables
    --
    -- <https://github.com/well-typed/hs-bindgen/issues/828>
  | ParseUnsupportedTLS

    -- | Variable declaration
  | ParseUnknownStorageClass (SimpleEnum CX_StorageClass)

    -- | Fully defined global variables and functions with external linkage.
    --
    -- Such definitions can lead to duplicate symbols (linker errors) if they
    -- are included more than once in the same program. See the manual section
    -- on globals for details.
    --
    -- Duplicate symbols can also exist across multiple shared libraries as long
    -- as these symbols have public visiblity. However, in such cases the linker
    -- will pick one according to the rules of linker symbol interposition,
    -- rather than throw a linker error. It can be suprising for users if the
    -- linker picks an unexpected definition for the symbol they are
    -- referencing. So, if a symbol has non-public visibility, the risk of such
    -- surprises is mitigated somewhat. See the "Visibility" section in the
    -- manual for more details.
  | ParsePotentialDuplicateSymbol
      -- | The symbol has public visibility
      Bool

    -- | A function declaration or global variable declaration has a problematic
    -- case of non-public visiblity that can lead to linker errors if the symbol
    -- is defined in a shared library.
    --
    -- In such cases, we emit this message. Arguably declarations like these are
    -- a bug in the C library, given the way that header files are @#include@d
    -- in other header and body files.
    --
    -- Concretely, a linker error can occur for a declarated symbol if it:
    -- 1. has non-public visibility,
    -- 2. has external linkage, and
    -- 3. is not a definition (and there is no definition elsewhere in the
    --    header).
    --
    -- For example:
    --
    -- > extern void __attribute__ ((visibility ("hidden"))) f (void);
    -- > extern int __attribute__ ((visibility ("hidden"))) i;
  | ParseNonPublicVisibility

    -- | A function declaration was encountered where the type of the function
    -- is typedef reference. This is not yet supported by hs-bindgen.
    --
    -- For example:
    --
    -- > typedef int int2int(int);
    -- > extern int2int foo;
    --
    -- <https://github.com/well-typed/hs-bindgen/issues/1034>
  | ParseFunctionOfTypeTypedef

    -- | Inform the user that we this declaration is deprecated. Maybe they want
    -- to de-select deprecated declarations?
  | ParseDeprecated
  deriving stock (Show, Eq, Ord)

instance PrettyForTrace DelayedParseMsg where
  prettyForTrace = \case
      ParseUnsupportedType err -> noBindingsGenerated $
        prettyForTrace err
      ParseUnsupportedImplicitFields -> noBindingsGenerated $
        "unsupported implicit fields"
      ParseUnexpectedAnonInSignature -> noBindingsGenerated $
          "unexpected anonymous declaration in function signature"
      ParseUnexpectedAnonInExtern -> noBindingsGenerated $
          "unexpected anonymous declaration in global variable"
      ParseUnsupportedTLS -> noBindingsGenerated $
        "unsupported thread-local variable"
      ParseUnknownStorageClass storage -> noBindingsGenerated $ PP.hsep [
          "unsupported storage class"
        , PP.showToCtxDoc storage
        ]
      ParsePotentialDuplicateSymbol isPublic -> PP.hcat $ [
            "Bindings may result in duplicate symbols; "
          , "consider using 'static' or 'extern'"
          ] ++
          if isPublic
          then [
              "; "
            , "or if that is not an option, consider attributing hidden "
            , "visibility to the symbol"
            ]
          else []
      ParseNonPublicVisibility -> PP.hsep [
          "Bindings may result in linker errors"
        , "because the symbol has non-public visibility"
        ]
      ParseFunctionOfTypeTypedef -> noBindingsGenerated $
        "unsupported function declared with a typedef type"
      ParseDeprecated ->
        "Declaration is deprecated; you may want to de-select it"
    where
      noBindingsGenerated :: CtxDoc -> CtxDoc
      noBindingsGenerated reason = PP.hcat [ "no bindings generated: ", reason ]

-- | Unsupported features are warnings, because we skip over them
instance IsTrace Level DelayedParseMsg where
  getDefaultLogLevel = \case
      ParseUnsupportedType err         -> getDefaultLogLevel err
      ParseUnsupportedImplicitFields{} -> Warning
      ParseUnexpectedAnonInSignature{} -> Warning
      ParseUnexpectedAnonInExtern{}    -> Warning
      ParseUnsupportedTLS{}            -> Warning
      ParseUnknownStorageClass{}       -> Warning
      ParsePotentialDuplicateSymbol{}  -> Notice
      ParseNonPublicVisibility{}       -> Warning
      ParseFunctionOfTypeTypedef{}     -> Warning
      ParseDeprecated                  -> Notice
  getSource  = const HsBindgen
  getTraceId = const "parse-delayed"
