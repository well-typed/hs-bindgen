-- | Parse messages
module HsBindgen.Frontend.Pass.Parse.Msg (
    -- * Immediate parse messages
    ImmediateParseMsg(..)

    -- * Delayed parse messages
  , DelayedParseMsg(..)
  ) where

import Control.Exception (Exception (..))
import Foreign.C (CInt)
import Text.SimplePrettyPrint ((><))
import Text.SimplePrettyPrint qualified as PP

import Clang.Enum.Simple
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Errors
import HsBindgen.Frontend.Naming (CTagKind)
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
    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1820>
    -- | We failed to parse a declaration that is required for scoping.
    ParseOfDeclarationRequiredForScopingFailed

  deriving stock (Show, Eq, Ord, Generic)

instance PrettyForTrace ImmediateParseMsg where
  prettyForTrace = \case
      ParseOfDeclarationRequiredForScopingFailed -> PP.hsep [
          "Parse of declaration required for scoping failed;"
        , "the failed declaration may be required when parsing"
        , "other declarations containing macro replacements"
        ]

instance IsTrace Level ImmediateParseMsg where
  getDefaultLogLevel = \case
      ParseOfDeclarationRequiredForScopingFailed{} -> Info
  getSource  = const HsBindgen
  getTraceId = const "parse-immediate"

{-------------------------------------------------------------------------------
  Delayed parse messages
-------------------------------------------------------------------------------}

-- Note to developers: We order delayed parse message constructors by
-- 1. Recursive constructors come first
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
  | ParseDeclarationOutOfScope CTagKind Text

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

    -- | Struct with implicit fields
  | ParseUnsupportedImplicitFields

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
  deriving stock (Show, Eq, Ord, Generic)

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
      ParseUnsupportedAnonInExtern ->
        "Unexpected anonymous declaration in global variable"
      ParseUnsupportedAnonInSignature ->
        "Unexpected anonymous declaration in function signature"
      ParseUnsupportedBuiltin name ->
        "Unsupported built-in " >< PP.show name
      ParseUnsupportedFloat128 ->
        "Unsupported float128"
      ParseUnsupportedImplicitFields ->
        "Unsupported implicit fields"
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
      ParseDeclarationOutOfScope _kind name -> PP.hcat [
            "Unexpected out of scope struct/union declaration '"
          , PP.text name
          , "'"
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

-- | Unsupported features are warnings
instance IsTrace Level DelayedParseMsg where
  getDefaultLogLevel = \case
      ParseUnderlyingTypeFailed _ err   -> getDefaultLogLevel err
      ParsePotentialDuplicateSymbol{}   -> Notice
      ParseDeclarationOutOfScope{}      -> Warning
      ParseFunctionOfTypeTypedef{}      -> Warning
      ParseInvalidLinkage               -> Warning
      ParseInvalidVisibility            -> Warning
      ParseNonPublicVisibility{}        -> Warning
      ParseUnknownCursorAvailability{}  -> Warning
      ParseUnknownStorageClass{}        -> Warning
      ParseUnsupportedAnonInExtern{}    -> Warning
      ParseUnsupportedAnonInSignature{} -> Warning
      ParseUnsupportedBuiltin{}         -> Warning
      ParseUnsupportedFloat128          -> Warning
      ParseUnsupportedImplicitFields{}  -> Warning
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
  getTraceId = const "parse"
