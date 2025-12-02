module HsBindgen.Frontend.Pass.Parse.IsPass (
    Parse
    -- * Typedefs
  , OrigTypedefRef(..)
    -- * Macros
  , UnparsedMacro(..)
  , ReparseInfo(..)
  , getUnparsedMacro
    -- * Trace messages
  , ParseSuccess(..)
  , ParseNotAttemptedReason(..)
  , ParseNotAttempted(..)
  , ParseFailure(..)
  , ParseResult(..)
  , getParseResultDecl
  , getParseResultLoc
  , getParseResultDeclId
  , parseSucceed
  , parseSucceedWith
  , parseDoNotAttempt
  , parseFail
  , RequiredForScoping(..)
  , ParseTypeExceptionContext(..)
  , ImmediateParseMsg(..)
  , AttachedParseMsg(..)
  , DelayedParseMsg(..)
  ) where

import Text.SimplePrettyPrint (CtxDoc, ($$), (><))
import Text.SimplePrettyPrint qualified as PP

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import HsBindgen.Frontend.AST.Coerce
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
  AnnParse "StructField"     = ReparseInfo
  AnnParse "UnionField"      = ReparseInfo
  AnnParse "Typedef"         = ReparseInfo
  AnnParse "Function"        = ReparseInfo
  AnnParse _                 = NoAnn

instance IsPass Parse where
  type Id           Parse = C.PrelimDeclId
  type FieldName    Parse = C.Name
  type ArgumentName Parse = Maybe C.Name
  type TypedefRef   Parse = OrigTypedefRef Parse
  type MacroBody    Parse = UnparsedMacro
  type ExtBinding   Parse = Void
  type Ann ix       Parse = AnnParse ix
  type Msg          Parse = ImmediateParseMsg

{-------------------------------------------------------------------------------
  Typedefs
-------------------------------------------------------------------------------}

-- A typedef reference.
--
-- In the example C code below, the type of global variable @x@ is a reference
-- to the typedef @bar@, which is in turn a reference to the typedef @foo@,
-- which is in turn a @const int@.
--
-- > typedef const int foo;
-- > typedef foo bar;
-- > bar x;
--
-- In Haskell, the reference to @bar@ records the name of the reference but also
-- the underlying type of @bar@, which is a reference to the typedef @foo@. In
-- turn, the reference to @foo@ records the name of the reference but also the
-- underlying type of @foo@, which is @const int@. The Haskell representation of
-- the type of variable @x@ will look roughly like:
--
-- > OrigTypedefRef "bar" (OrigTypedefRef "foo" "const int")
--
-- The underlying type is included so that it can be inspected locally without
-- requiring global information. This is primarily used in the backend, where
-- Haskell binding generation depends on what types look like with all typedefs
-- erased.
data OrigTypedefRef p = OrigTypedefRef
    -- | Name of the referenced typedef declaration
    C.Name
    -- | The underlying type of the referenced typedef declaration
    --
    -- NOTE: the underlying type can arbitrarily reference other types,
    -- including typedefs that we have not parsed. Use the underlying type with
    -- care!
    (C.Type p)
  deriving stock (Show, Eq, Generic)

instance (
      CoercePassId p p'
    , ArgumentName p ~ ArgumentName p'
    , CoercePassTypedefRef p p'
    , ExtBinding p ~ ExtBinding p'
    ) => CoercePass OrigTypedefRef p p' where
  coercePass (OrigTypedefRef n uTy) = OrigTypedefRef n (coercePass uTy)

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

data ParseSuccess = ParseSuccess {
      psQualPrelimDeclId :: C.QualPrelimDeclId
    , psDecl             :: C.Decl Parse
    , psAttachedMsgs     :: [AttachedParseMsg DelayedParseMsg]
    }
  deriving stock (Show, Generic)

instance PrettyForTrace ParseSuccess where
  prettyForTrace ParseSuccess{..} =
    if null psAttachedMsgs then
      PP.hang "Parse success:" 2 $
        prettyForTrace $
          C.Located psDecl.declInfo.declLoc psQualPrelimDeclId
    else
      PP.hang "Parse success with messages:" 2 $
        PP.vcat $
          map prettyForTrace psAttachedMsgs

-- | Why did we not attempt to parse a declaration?
data ParseNotAttemptedReason =
    -- | We do not parse builtin declarations.
    DeclarationBuiltin

    -- | We unexpectedly did not attempt to parse a declaration because it is
    -- reported "unavailable".
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

instance PrettyForTrace ParseNotAttemptedReason where
  prettyForTrace x = case x of
    DeclarationBuiltin       -> "Builtin declaration"
    DeclarationUnavailable   -> "Declaration is 'unavailable' on this platform"
    ParsePredicateNotMatched -> "Parse predicate did not match"

-- | Declarations we did not attempt to parse
--
-- We need this information when selecting declarations: Does the user want to
-- select declarations we did not attempt to parse?
newtype ParseNotAttempted = ParseNotAttempted {
      unParseNotAttempted :: AttachedParseMsg ParseNotAttemptedReason
    }
  deriving stock    (Eq, Show, Generic)

instance PrettyForTrace ParseNotAttempted where
  prettyForTrace (ParseNotAttempted x) =
    PP.hang "Parse not attempted: " 2 $ prettyForTrace x

-- | Declarations that match the parse predicate but that we fail to parse and
-- reify
--
-- We need this information when selecting declarations: Does the user want to
-- select declarations, we have failed to parse?
newtype ParseFailure = ParseFailure {
      unParseFailure :: AttachedParseMsg DelayedParseMsg
    }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (IsTrace Level)

instance PrettyForTrace ParseFailure where
  prettyForTrace (ParseFailure x) =
    PP.hang "Parse failure:" 2 $
      prettyForTrace x

data ParseResult =
    ParseResultSuccess      ParseSuccess
  | ParseResultNotAttempted ParseNotAttempted
  | ParseResultFailure      ParseFailure
  deriving stock (Show, Generic)
  deriving anyclass (PrettyForTrace)

getParseResultDecl :: ParseResult -> Either ParseResult (C.Decl Parse)
getParseResultDecl = \case
    ParseResultSuccess ParseSuccess{..} -> Right psDecl
    other                               -> Left other

getParseResultLoc :: ParseResult -> SingleLoc
getParseResultLoc = \case
    ParseResultSuccess       ParseSuccess{psDecl} -> psDecl.declInfo.declLoc
    ParseResultNotAttempted (ParseNotAttempted m) -> m.loc
    ParseResultFailure      (ParseFailure m)      -> m.loc

getParseResultDeclId :: ParseResult -> QualPrelimDeclId
getParseResultDeclId = \case
    ParseResultSuccess       ParseSuccess{..}     -> psQualPrelimDeclId
    ParseResultNotAttempted (ParseNotAttempted x) -> x.declId
    ParseResultFailure      (ParseFailure x)      -> x.declId

parseSucceed :: C.Decl Parse -> ParseResult
parseSucceed = parseSucceedWith []

parseSucceedWith :: [DelayedParseMsg] -> C.Decl Parse -> ParseResult
parseSucceedWith msgs decl =
    ParseResultSuccess $ ParseSuccess qualPrelimDeclId decl $
      map (AttachedParseMsg qualPrelimDeclId declLoc declAvailability) msgs
  where
    qualPrelimDeclId = C.declQualPrelimDeclId decl
    C.DeclInfo{..} = decl.declInfo

parseDoNotAttempt ::
     C.DeclInfo Parse
  -> C.NameKind
  -> ParseNotAttemptedReason
  -> ParseResult
parseDoNotAttempt C.DeclInfo{..} kind reason =
    ParseResultNotAttempted $
      ParseNotAttempted $
        AttachedParseMsg
          (C.qualPrelimDeclIdSafe declId kind)
          declLoc
          declAvailability
          reason

parseFail ::
  C.DeclInfo Parse -> C.NameKind -> DelayedParseMsg -> ParseResult
parseFail info kind msg = ParseResultFailure $ ParseFailure $
      AttachedParseMsg
        (C.qualPrelimDeclId info.declId kind)
        info.declLoc
        info.declAvailability
        msg

-- | We always need to parse declarations required for scoping
data RequiredForScoping = RequiredForScoping | NotRequiredForScoping
  deriving stock (Show, Eq)

data ParseTypeExceptionContext = ParseTypeExceptionContext {
      contextInfo               :: C.DeclInfo Parse
    , contextNameKind           :: NameKind
    , contextRequiredForScoping :: RequiredForScoping
    }
  deriving stock (Show)

-- instance PrettyForTrace ParseTypeExceptionContext where
--   prettyForTrace (ParseTypeExceptionContext info kind) =
--     prettyForTrace info <+> ", name kind: " <+> prettyForTrace kind

-- | Parse messages that we emit immediately
--
-- For example, if we can not attach messages to declarations, we emit them
-- directly while parsing.
data ImmediateParseMsg =
    -- | Declaration availability can not be determined.
    --
    -- That is 'Clang.LowLevel.Core.clang_getCursorAvailability' does not
    -- provide a valid 'Clang.LowLevel.Core.CXAvailabilityKind'.
    ParseUnknownCursorAvailability (C.DeclInfo Parse) (SimpleEnum CXAvailabilityKind)
    -- | We failed to parse a declaration that is required for scoping.
  | ParseOfDeclarationRequiredForScopingFailed (C.DeclInfo Parse) ParseTypeException
  deriving stock (Show)

instance PrettyForTrace ImmediateParseMsg where
  prettyForTrace = \case
      ParseUnknownCursorAvailability info simpleKind -> PP.hsep [
          prettyForTrace info
        , "unknown declaration availability:"
        , PP.showToCtxDoc simpleKind
        ]
      ParseOfDeclarationRequiredForScopingFailed info err -> PP.hsep [
          prettyForTrace info
        , "parse of declaration required for scoping failed:"
        ] $$ (PP.nest 2 $ prettyForTrace err)

instance IsTrace Level ImmediateParseMsg where
  getDefaultLogLevel = \case
      ParseUnknownCursorAvailability{}             -> Notice
      ParseOfDeclarationRequiredForScopingFailed{} -> Info
  getSource  = const HsBindgen
  getTraceId = const "parse-immediate"

data AttachedParseMsg a = AttachedParseMsg {
    declId       :: QualPrelimDeclId
  , loc          :: SingleLoc
  , availability :: C.Availability
  , msg          :: a
  }
  deriving stock (Eq, Show, Generic)

instance PrettyForTrace a => PrettyForTrace (AttachedParseMsg a) where
  prettyForTrace (AttachedParseMsg i l _ x) =
    PP.hang (prettyForTrace (C.Located l i) >< ":") 2 $ prettyForTrace x

instance IsTrace Level a => IsTrace Level (AttachedParseMsg a) where
  getDefaultLogLevel = getDefaultLogLevel . msg
  getSource          = getSource . msg
  getTraceId         = getTraceId . msg

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
  deriving stock (Show, Eq, Ord, Generic)

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
    where
      noBindingsGenerated :: CtxDoc -> CtxDoc
      noBindingsGenerated reason = PP.hsep ["No bindings generated:", reason]

-- | Unsupported features are warnings
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
  getSource  = const HsBindgen
  getTraceId = const "parse-delayed"
