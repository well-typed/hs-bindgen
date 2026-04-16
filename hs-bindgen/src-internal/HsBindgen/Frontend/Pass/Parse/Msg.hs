-- | Parse messages
module HsBindgen.Frontend.Pass.Parse.Msg (
    -- * Immediate parse messages
    ImmediateParseMsg(..)

    -- * Delayed parse messages
  , DelayedParseMsg(..)
  , ParseImplicitFieldsMsg(..)
  ) where

import Control.Exception (Exception (..))
import Foreign.C (CInt)
import Text.SimplePrettyPrint ((><))
import Text.SimplePrettyPrint qualified as PP

import C.Expr.Parse qualified as CExpr.DSL

import Clang.Enum.Simple
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Errors
import HsBindgen.Frontend.LanguageC.Error qualified as LanC
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (AnonId, PrelimDeclId)
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Immediate parse messages
-------------------------------------------------------------------------------}

-- | Parse messages that we emit immediately
--
-- Reasons for immediate emission:
--
-- - The info/debug message is useful to developers
--
-- - The declaration we fail to parse may affect other declarations
data ImmediateParseMsg =
    -- | We do not support getting macro expansions for declarations spanning
    --   multiple files.
    ParseGetMacroExpansionsMultipleFiles (Range SingleLoc)

    -- | At a macro expansion site, we failed to get the name of the expanded
    --   macro.
  | ParseMacroExpansionNoMacroName

    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1820>
    -- | We failed to parse a declaration that is required for scoping.
  | ParseOfDeclarationRequiredForScopingFailed

    -- | Sequence numbers were populated, using 'clang_isBeforeInTranslationUnit'
    --
    -- Sequence numbers record the order in which declarations appear in the
    -- translation unit.  Populating them requires Clang >= 20.1.
  | ParseSeqNrPopulated

    -- | Sequence numbers could not be populated
    --
    -- 'clang_isBeforeInTranslationUnit' is not available; this requires
    -- Clang >= 20.1.
  | ParseSeqNrUnavailable

  deriving stock (Show, Eq, Ord, Generic)

instance PrettyForTrace ImmediateParseMsg where
  prettyForTrace = \case
      ParseGetMacroExpansionsMultipleFiles range -> PP.hsep [
          "Could not get macro expansions"
        , "(declaration spans multiple files):"
        , PP.show range
        ]
      ParseMacroExpansionNoMacroName ->
        "Could not obtain macro name at macro expansion location"
      ParseOfDeclarationRequiredForScopingFailed -> PP.hsep [
          "Parse of declaration required for scoping failed;"
        , "the failed declaration may be required when parsing"
        , "other declarations containing macro replacements"
        ]
      ParseSeqNrPopulated -> PP.hsep [
          "Sequence order of declarations populated"
        , "(requires Clang >= 20.1)"
        ]
      ParseSeqNrUnavailable -> PP.hsep [
          "Sequence order of declarations unavailable:"
        , "clang_isBeforeInTranslationUnit requires Clang >= 20.1"
        ]

instance IsTrace Level ImmediateParseMsg where
  getDefaultLogLevel = \case
      ParseGetMacroExpansionsMultipleFiles{}       -> Warning
      ParseMacroExpansionNoMacroName{}             -> Bug
      ParseOfDeclarationRequiredForScopingFailed{} -> Info
      ParseSeqNrPopulated{}                        -> Info
      ParseSeqNrUnavailable{}                      -> Info
  getSource  = const HsBindgen
  getTraceId = \case
    ParseMacroExpansionNoMacroName -> "parse-immediate-macro"
    _otherwise                     -> "parse-immediate"

{-------------------------------------------------------------------------------
  Delayed parse messages
-------------------------------------------------------------------------------}

-- Note to developers: We order delayed parse message constructors by
-- 1. Recursive/nested constructors come first
-- 2. Severity; debug messages come first
-- 3. Constructor name

-- | Delayed parse messages
--
-- We emit these parse messages only when we attempt to select the attached
-- declaration.
--
-- We distinguish between \"unsupported\", which refers to C features that one
-- could reasonably expect to be supported eventually, and \"unexpected\", for C
-- input we are not prepared for.
data DelayedParseMsg =
    -- | Recursive case; we failed to parse the target of a @typedef@ with a
    --   delayed parse message.
    ParseUnderlyingTypeFailed PrelimDeclId DelayedParseMsg

    -- | We tried to parse an implicit field for a struct or union object, but
    -- it was unsuccessful
  | ParseImplicitFieldFailed ParseImplicitFieldsMsg

  | ParseMacroEmpty PrelimDeclId [Token TokenSpelling]

    -- | We could not parse the macro (macro def sites)
  | ParseMacroErrorParse CExpr.DSL.MacroParseError

    -- | We could not reparse a fragment of C (to recover macro use sites)
  | ParseMacroErrorReparse LanC.Error

    -- | While reparsing a declaration with a macro expansion, we do not know
    --   the type of an expanded macro.
  | ParseMacroReparseUnknownType Text

    -- | Fully defined global variables and functions with external linkage.
    --
    -- Such definitions can lead to duplicate symbols (linker errors) if they
    -- are included more than once in the same program. See the manual section
    -- on globals for details.
    --
    -- Duplicate symbols can also exist across multiple shared libraries as long
    -- as these symbols have public visibility. However, in such cases the
    -- linker will pick one according to the rules of linker symbol
    -- interposition, rather than throw a linker error. It can be surprising for
    -- users if the linker picks an unexpected definition for the symbol they
    -- are referencing. So, if a symbol has non-public visibility, the risk of
    -- such surprises is mitigated somewhat. See the "Visibility" section in the
    -- manual for more details.
  | ParsePotentialDuplicateSymbol
      -- | The symbol has public visibility
      Bool

    -- | Struct/union tag first declared inside a function prototype, e.g.
    -- @void f(struct foo* arg);@
    --
    -- Might indicate a missing @#include@ in the header.
  | ParseDeclarationNotVisible CTagKind Text

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

  | ParseInvalidLinkage

  | ParseInvalidVisibility

    -- | Failed to parse a declaration because some of its nested declarations
    -- failed to parse
    --
    -- For example, to successfully parse a struct (or union), all of its
    -- nested declarations have to be parsed successfully as well. If any
    -- nested declaration fails to parse, then we could miss seeing some
    -- (implicit) fields, and we can't generate Haskell bindings for structs
    -- (or unions) with an incomplete list of fields.
  | ParseNestedDeclsFailed

    -- | A function declaration or global variable declaration has a problematic
    -- case of non-public visibility that can lead to linker errors if the
    -- symbol is defined in a shared library.
    --
    -- In such cases, we emit this message. Arguably declarations like these are
    -- a bug in the C library, given the way that header files are @#include@d
    -- in other header and body files.
    --
    -- Concretely, a linker error can occur for a declared symbol if it:
    --
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

    -- | Declaration availability can not be determined.
    --
    -- That is 'Clang.LowLevel.Core.clang_getCursorAvailability' does not
    -- provide a valid 'Clang.LowLevel.Core.CXAvailabilityKind'.
  | ParseUnknownCursorAvailability (SimpleEnum CXAvailabilityKind)

    -- | Variable declaration
  | ParseUnknownStorageClass (SimpleEnum CX_StorageClass)

    -- | Unexposed type
    --
    -- In some cases, @libclang@ does not expose information about a type.  For
    -- example, this happens with the standard library declaration of @malloc@
    -- and other memory allocation functions, as LLVM/Clang handles them
    -- specially in order to optimize memory allocation.
  | ParseUnexposedType

    -- | Unsupported anonymous declaration inside @extern@
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
    -- we detect the situation and warn the user in @hs-bindgen@.
    --
    -- (As of C23, the situation is different for /named/ structs: multiple uses
    -- of a struct with the same name are considered compatible as of
    -- WG14-N3037.)
  | ParseUnsupportedAnonInExtern

    -- | Unsupported anonymous declaration inside function signature
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
  | ParseUnsupportedAnonInSignature

    -- | Clang built-in declaration
  | ParseUnsupportedBuiltin Text

    -- | We do not support @float128@
  | ParseUnsupportedFloat128

  | ParseUnsupportedLinkage String CXLinkageKind

    -- | We do not support @long double@
  | ParseUnsupportedLongDouble

    -- | Thread local variables
    --
    -- <https://github.com/well-typed/hs-bindgen/issues/828>
  | ParseUnsupportedTLS

    -- | We do not support variadic (varargs) functions
  | ParseUnsupportedVariadicFunction

    -- | Unusable anonymous declaration
    --
    -- When an unusable declaration appears in some outer declaration (say a
    -- function signature), we will fail to \"parse\" that outer declaration
    -- (actually this happens as a separate post-processing step in
    -- @AssignAnonIds@). We record the identifier of the anonymous declaration
    -- here (that is, it's source location); the identifier of the outer
    -- declaration is recorded in the encloding 'ParseResult'.
  | ParseUnusableAnonDecl AnonId

  | ParseExpectedFunctionType String

    -- | Complex types can only be defined using primitive types, e.g.
    -- @double complex@. @struct Point complex@ is not allowed.
  | ParseUnexpectedComplexType CXType

    -- | We encountered an unexpected cursor kind
    --
    -- Similar comments apply as for 'UnexpectedTypeKind'.
  | ParseUnexpectedCursorKind (Either CInt CXCursorKind)

  | ParseUnexpectedLinkage (Either CInt CXLinkageKind)

    -- | We encountered an unexpected type kind
    --
    -- This is always a bug in hs-bindgen: if this kind of type is unsupported,
    -- we should explicitly check for it and throw an appropriate exception.
    --
    -- If this is a @Left@ value, it means that our @libclang@ bindings are
    -- incomplete.
  | ParseUnexpectedTypeKind (Either CInt CXTypeKind)

  | ParseUnexpectedVisibility (Either CInt CXLinkageKind)

  | ParseNoMainHeadersException String SourcePath
  deriving stock (Show, Generic)

instance Exception DelayedParseMsg where
  displayException = PP.renderCtxDoc (PP.mkContext 100) . prettyForTrace

instance PrettyForTrace DelayedParseMsg where
  prettyForTrace = \case
      ParseUnderlyingTypeFailed name err -> PP.hcat [
          "Parse failure of underlying type of typedef "
        , prettyForTrace name
        , ": "
        , prettyForTrace err
        ]
      ParseImplicitFieldFailed reason -> PP.hsep [
          "Failed to parse an implicit struct or union field"
        , "referencing an anonymous object. Reason: "
        , prettyForTrace reason
        ]
      ParseMacroEmpty name tokens -> PP.hsep [
          "Ignoring empty macro"
        , prettyForTrace name >< ":"
        , PP.show tokens
        ]
      ParseMacroErrorParse err -> PP.vcat [
          "Could not parse macro:"
        , PP.nest 2 $ prettyMacroParseError err
        ]
      ParseMacroErrorReparse x -> PP.hsep [
          "Failed to reparse: "
        , prettyForTrace x
        ]
      ParseMacroReparseUnknownType x -> PP.hsep [
          "During reparse:"
        , "Unknown type of expanded macro"
        , PP.text x
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
      ParseFunctionOfTypeTypedef ->
        "Unsupported function declared with a typedef type"
      ParseInvalidLinkage ->
        "Invalid linkage (CXLinkage_Invalid)"
      ParseInvalidVisibility ->
        "Invalid visibility (CXVisibility_Invalid)"
      ParseNestedDeclsFailed -> PP.hsep [
          "Failed to parse a declaration because some of its nested declarations"
        , "failed to parse"
        ]
      ParseNonPublicVisibility -> PP.hsep [
          "Bindings may result in linker errors"
        , "because the symbol has non-public visibility"
        ]
      ParseUnknownCursorAvailability simpleKind -> PP.hsep [
          "Unknown declaration cursor availability:"
        , PP.show simpleKind
        ]
      ParseUnknownStorageClass storage -> PP.hsep [
          "Unsupported storage class"
        , PP.show storage
        ]
      ParseUnexposedType ->
        "Unexposed type"
      ParseUnsupportedAnonInExtern ->
        "Unexpected anonymous declaration in global variable"
      ParseUnsupportedAnonInSignature ->
        "Unexpected anonymous declaration in function signature"
      ParseUnsupportedBuiltin name ->
        "Unsupported built-in " >< PP.show name
      ParseUnsupportedFloat128 ->
        "Unsupported float128"
      ParseUnsupportedLinkage comment linkage -> PP.hcat [
          "Unsupported linkage: "
        , PP.show linkage
        , " ("
        , PP.string comment
        , ")"
        ]
      ParseUnsupportedLongDouble ->
        "Unsupported long double"
      ParseUnsupportedTLS ->
        "Unsupported thread-local variable"
      ParseUnsupportedVariadicFunction ->
        "Unsupported variadic (varargs) function"
      ParseUnusableAnonDecl anonId -> PP.hsep [
          "Unusable anonymous declaration "
        , prettyForTrace anonId
        ]
      ParseDeclarationNotVisible kind name -> PP.hcat [
            "Declaration of '"
          , PP.text (cTagKindPrefix kind)
          , " "
          , PP.text name
          , "' will not be visible outside of this function"
          ]
      ParseExpectedFunctionType ty -> PP.hsep [
          "Expected function type, but got"
        , PP.string ty
        ]
      ParseUnexpectedComplexType ty ->
        unexpected $ "complex type " >< PP.show ty
      ParseUnexpectedCursorKind x ->
        unexpected $ "cursor kind " >< either PP.show PP.show x
      ParseUnexpectedLinkage linkage -> PP.hcat [
          "Unexpected linkage: "
        , PP.show linkage
        ]
      ParseUnexpectedTypeKind x ->
        unexpected $ "type kind " >< either PP.show PP.show x
      ParseUnexpectedVisibility visibility -> PP.hcat [
          "Unexpected visibility: "
        , PP.show visibility
        ]
      ParseNoMainHeadersException why whr -> PP.hsep [
          "Could not determine main headers:"
        , PP.string why
        , "at"
        , PP.string $ getSourcePath whr
        ]
    where
      unexpected :: PP.CtxDoc -> PP.CtxDoc
      unexpected msg = PP.vcat [
            "Unexpected " >< msg
          , PP.string pleaseReport
          ]

      prettyMacroParseError :: CExpr.DSL.MacroParseError -> PP.CtxDoc
      prettyMacroParseError err = PP.renderedLines $ \_maxWidth -> lines err.reparseError

-- | Unsupported features are warnings
instance IsTrace Level DelayedParseMsg where
  getDefaultLogLevel = \case
      ParseUnderlyingTypeFailed _ x     -> getDefaultLogLevel x
      ParseImplicitFieldFailed    x     -> getDefaultLogLevel x
      ParseMacroEmpty{}                 -> Info
      ParseMacroErrorParse{}            -> Info
      ParseMacroErrorReparse{}          -> Info
      ParseMacroReparseUnknownType{}    -> Info
      ParsePotentialDuplicateSymbol{}   -> Notice
      ParseDeclarationNotVisible{}      -> Warning
      ParseFunctionOfTypeTypedef{}      -> Warning
      ParseInvalidLinkage               -> Warning
      ParseInvalidVisibility            -> Warning
      ParseNestedDeclsFailed{}          -> Warning
      ParseNonPublicVisibility{}        -> Warning
      ParseUnknownCursorAvailability{}  -> Warning
      ParseUnknownStorageClass{}        -> Warning
      ParseUnexposedType                -> Warning
      ParseUnsupportedAnonInExtern{}    -> Warning
      ParseUnsupportedAnonInSignature{} -> Warning
      ParseUnsupportedBuiltin{}         -> Warning
      ParseUnsupportedFloat128          -> Warning
      ParseUnsupportedLinkage{}         -> Warning
      ParseUnsupportedLongDouble        -> Warning
      ParseUnsupportedTLS{}             -> Warning
      ParseUnsupportedVariadicFunction  -> Warning
      ParseUnusableAnonDecl{}           -> Warning
      ParseExpectedFunctionType{}       -> Bug
      ParseUnexpectedComplexType{}      -> Bug
      ParseUnexpectedCursorKind{}       -> Bug
      ParseUnexpectedLinkage{}          -> Bug
      ParseUnexpectedTypeKind{}         -> Bug
      ParseUnexpectedVisibility{}       -> Bug
      ParseNoMainHeadersException{}     -> Error
  getSource  = const HsBindgen
  getTraceId = \case
      ParseImplicitFieldFailed x     -> "parse-" <> getTraceId x
      ParseMacroEmpty{}              -> "parse-macro"
      ParseMacroErrorParse{}         -> "parse-macro"
      ParseMacroErrorReparse{}       -> "parse-macro"
      ParseMacroReparseUnknownType{} -> "parse-macro"
      _ -> "parse"

{-------------------------------------------------------------------------------
  Delayed parse messages: implicit fields
-------------------------------------------------------------------------------}

data ParseImplicitFieldsMsg =
    -- | Unsupported empty anonymous nested struct or union
    --
    -- Anonymous nested structs and unions need to have at least one named
    -- member for parsing of implicit fields to succeed.
    UnsupportedEmptyAnon
    -- | An unexpected exception was thrown when using @clang_Type_getOffsetOf@
    --
    -- This is likely a bug in implicit field parsing.
  | UnexpectedClangOffsetOfException Text String
    -- | An unexpected non-zero field offset for an implicit field of a union
    -- was computed
    --
    -- This is likely a bug in implicit field parsing.
  | UnexpectedNonZeroFieldOffset Int
  deriving stock (Show, Eq, Ord, Generic)


instance PrettyForTrace ParseImplicitFieldsMsg where
  prettyForTrace = \case
      UnsupportedEmptyAnon -> PP.hsep [
          "Unsupported empty nested anonymous union or struct:"
        , "it should have at least one named field"
        ]
      UnexpectedClangOffsetOfException field exc -> PP.hsep [
          "Unexpected exception when using clang_Type_getOffsetOf"
        , "with field name", PP.text field, ":", PP.string exc
        ]
      UnexpectedNonZeroFieldOffset off -> PP.hsep [
          "Unexpected non-zero field offset for an implicit field of a union:"
        , PP.show off
        ]

-- | Unsupported features are warnings
instance IsTrace Level ParseImplicitFieldsMsg where
  getDefaultLogLevel = \case
      UnsupportedEmptyAnon{}              -> Warning
      UnexpectedNonZeroFieldOffset{}      -> Bug
      UnexpectedClangOffsetOfException{}  -> Bug
  getSource  = const HsBindgen
  getTraceId = const "implicit-fields"
