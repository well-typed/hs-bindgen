module HsBindgen.Frontend.Pass.Parse.IsPass (
    Parse
  , ParseDeclMeta(..)
  , emptyParseDeclMeta
    -- * Macros
  , UnparsedMacro(..)
  , ReparseInfo(..)
  , getUnparsedMacro
    -- * Trace messages
  , ParseTypeExceptionContext(..)
  , ParseMsg(..)
  , ParseMsgKey(..)
  , ParseMsgs(..)
  , emptyParseMsgs
  , coerceParseMsgs
  , mapParseMsgs
  , recordParseMsg
  ) where

import Data.Map qualified as Map

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import HsBindgen.Frontend.AST.Coerce (CoercePass (..))
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming (NameKind)
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.NonParsedDecls (NonParsedDecls)
import HsBindgen.Frontend.NonParsedDecls qualified as NonParsedDecls
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Type.Monad
import HsBindgen.Imports
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint (CtxDoc, (<+>), (><))
import Text.SimplePrettyPrint qualified as PP

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
  type Msg          Parse = ParseMsg

{-------------------------------------------------------------------------------
  Information about the declarations
-------------------------------------------------------------------------------}

data ParseDeclMeta = ParseDeclMeta {
      parseDeclNonParsed :: NonParsedDecls
    , parseDeclParseMsg  :: ParseMsgs Parse
    }
  deriving stock (Show, Eq, Ord)

emptyParseDeclMeta :: ParseDeclMeta
emptyParseDeclMeta = ParseDeclMeta {
      parseDeclNonParsed = NonParsedDecls.empty
    , parseDeclParseMsg  = emptyParseMsgs
    }

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
      contextInfo :: C.DeclInfo Parse
    , contextNameKind :: NameKind
    }
  deriving stock (Show, Eq, Ord)

instance PrettyForTrace ParseTypeExceptionContext where
  prettyForTrace (ParseTypeExceptionContext info kind) =
    prettyForTrace info <+> ", name kind: " <+> prettyForTrace kind

-- | Parse messages
--
-- We distinguish between \"unsupported\", which refers to C features that one
-- could reasonably expect to be supported eventually, and \"unexpected\", for
-- strange C input.
data ParseMsg =
    -- | We excluded a declaration
    ParseExcluded (C.DeclInfo Parse)

    -- | Unsupported type
    --
    -- Since types don't necessarily have associated source locations, we
    -- instead record information about the enclosing declaration.
  | ParseUnsupportedType (C.DeclInfo Parse) ParseTypeException

    -- | Struct with implicit fields
  | ParseUnsupportedImplicitFields (C.DeclInfo Parse)

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
  | ParseUnexpectedAnonInSignature (C.DeclInfo Parse)

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
  | ParseUnexpectedAnonInExtern (C.DeclInfo Parse)

    -- | Thread local variables
    --
    -- <https://github.com/well-typed/hs-bindgen/issues/828>
  | ParseUnsupportedTLS (C.DeclInfo Parse)

    -- | Variable declaration
  | ParseUnknownStorageClass (C.DeclInfo Parse) (SimpleEnum CX_StorageClass)

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
    -- surprises is mitigated somewhat. See the section on Visibility in the
    -- manual for more details.
  | ParsePotentialDuplicateSymbol (C.DeclInfo Parse)
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
  | ParseNonPublicVisibility (C.DeclInfo Parse)
  deriving stock (Show, Eq, Ord)

instance PrettyForTrace ParseMsg where
  prettyForTrace = \case
      ParseExcluded info ->
          prettyForTrace info >< " excluded"
      ParseUnsupportedType info err -> noBindingsGenerated info $
          prettyForTrace err
      ParseUnsupportedImplicitFields info -> noBindingsGenerated info $
          "unsupported implicit fields"
      ParseUnexpectedAnonInSignature info -> noBindingsGenerated info $
          "unexpected anonymous declaration in function signature"
      ParseUnexpectedAnonInExtern info -> noBindingsGenerated info $
          "unexpected anonymous declaration in global variable"
      ParseUnsupportedTLS info -> noBindingsGenerated info $
          "unsupported thread-local variable"
      ParseUnknownStorageClass info storage -> noBindingsGenerated info $ PP.hsep [
          "unsupported storage class"
        , PP.showToCtxDoc storage
        ]
      ParsePotentialDuplicateSymbol info isPublic -> PP.hcat $ concat [
          [ "Bindings generated for "
          , prettyForTrace info
          , " may result in duplicate symbols; "
          , "consider using 'static' or 'extern';" ]
        , concat [ [
              " or if that is not an option, consider attributing hidden "
            , "visibility to the symbol"
            ]
          | isPublic
          ]
        ]
      ParseNonPublicVisibility info  -> PP.hcat [
          "Bindings generated for "
        , prettyForTrace info
        , " may result in linker errors because the symbol has non-public visibility"
        ]
    where
      noBindingsGenerated :: C.DeclInfo Parse -> CtxDoc -> CtxDoc
      noBindingsGenerated info reason = PP.hcat [
            "No bindings generated for "
          , prettyForTrace info
          , ": "
          , reason
          ]

-- | Unsupported features are warnings, because we skip over them
instance IsTrace Level ParseMsg where
  getDefaultLogLevel = \case
      ParseExcluded{}                  -> Info
      ParseUnsupportedType _ctxt err   -> getDefaultLogLevel err
      ParseUnsupportedImplicitFields{} -> Warning
      ParseUnexpectedAnonInSignature{} -> Warning
      ParseUnexpectedAnonInExtern{}    -> Warning
      ParseUnsupportedTLS{}            -> Warning
      ParseUnknownStorageClass{}       -> Warning
      ParsePotentialDuplicateSymbol{}  -> Notice
      ParseNonPublicVisibility{}       -> Warning
  getSource  = const HsBindgen
  getTraceId = const "parse"

{-------------------------------------------------------------------------------
  Location-specific parse messages
-------------------------------------------------------------------------------}

-- | We emit parse traces only when we select the corresponding declaration.
--
-- The 'ParseMsgKey' allows us to identify parse messages for selected and
-- parsed/reified declarations, as well as for declarations we wanted to select
-- but that were skipped during parse/reification. We call these latter
-- declarations "failed declarations".
newtype ParseMsgs p = ParseMsgs { unParseMsgs :: Map (ParseMsgKey p) [ParseMsg] }
deriving instance Show (Id p) => Show (ParseMsgs p)
deriving instance Eq (Id p)   => Eq (ParseMsgs p)
deriving instance Ord (Id p)  => Ord (ParseMsgs p)

data ParseMsgKey p = ParseMsgKey {
      parseMsgDeclLoc  :: SingleLoc
    , parseMsgDeclId   :: Id p
    , parseMsgDeclKind :: NameKind
    }
deriving stock instance Show (Id p) => Show (ParseMsgKey p)
deriving stock instance Eq (Id p)   => Eq (ParseMsgKey p)
deriving stock instance Ord (Id p)  => Ord (ParseMsgKey p)

instance (Id p ~ Id p') => CoercePass ParseMsgKey p p' where
  coercePass (ParseMsgKey l i k) = ParseMsgKey l i k

emptyParseMsgs :: ParseMsgs p
emptyParseMsgs = ParseMsgs $ Map.empty

coerceParseMsgs :: (Id p ~ Id p', Ord (ParseMsgKey p'))
  => ParseMsgs p -> ParseMsgs p'
coerceParseMsgs = ParseMsgs . Map.mapKeys coercePass . unParseMsgs

mapParseMsgs :: Ord (ParseMsgKey p')
  => (ParseMsgKey p -> ParseMsgKey p')
  -> ParseMsgs p
  -> ParseMsgs p'
mapParseMsgs f = ParseMsgs . Map.mapKeys f . unParseMsgs

recordParseMsg :: forall p. Ord (ParseMsgKey p)
  => C.DeclInfo p -> NameKind -> ParseMsg -> ParseMsgs p -> ParseMsgs p
recordParseMsg info kind trace =
   ParseMsgs . Map.alter (Just <$> add trace) key . unParseMsgs
  where
    key :: ParseMsgKey p
    key = ParseMsgKey (C.declLoc info) (C.declId info) kind

    add x Nothing   = [x]
    add x (Just xs) = x:xs
