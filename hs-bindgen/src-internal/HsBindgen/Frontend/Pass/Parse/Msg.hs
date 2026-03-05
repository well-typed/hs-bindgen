-- | Parse messages
module HsBindgen.Frontend.Pass.Parse.Msg (
    ParseMsg(..)

    -- * Immediate parse messages
  , ImmediateParseMsg(..)

    -- * Delayed parse messages
  , DelayedParseMsg(..)
  ) where

import Control.Exception
import Foreign.C
import Text.SimplePrettyPrint ((><))
import Text.SimplePrettyPrint qualified as PP

import Clang.Enum.Simple
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Errors
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (AnonId, PrelimDeclId)
import HsBindgen.Imports
import HsBindgen.Util.Tracer

-- | An immediate or delayed parse message
data ParseMsg = Immediate ImmediateParseMsg | Delayed DelayedParseMsg
  deriving stock (Show, Eq, Ord, Generic)

instance PrettyForTrace ParseMsg
instance IsTrace Level ParseMsg

instance Exception ParseMsg where
  displayException = PP.renderCtxDoc (PP.mkContext 100) . prettyForTrace

{-------------------------------------------------------------------------------
  Immediate parse messages
-------------------------------------------------------------------------------}

-- | Parse messages that we emit immediately
--
-- Reasons for immediate emission:
--
-- - The user may not care (they might not even select the declaration), but we
--   care (i.e., a bug or an unsupported feature)
--
-- - The declaration we fail to parse may affect other declarations
data ImmediateParseMsg =
    -- | Recursive case; we failed to parse the target of a @typedef@ with an
    --   immediate parse message.
    ParseUnexpectedUnderlyingType PrelimDeclId ImmediateParseMsg

    -- | Recursive case; we failed to parse a declaration that is required for scoping.
  | ParseOfDeclarationRequiredForScopingFailed ParseMsg

    -- | Recursive case; we failed to parse a declaration that may be required
    --   for scoping.
  | ParseOfDeclarationMaybeRequiredForScopingFailed ParseMsg

    -- | Declaration availability can not be determined.
    --
    -- That is 'Clang.LowLevel.Core.clang_getCursorAvailability' does not
    -- provide a valid 'Clang.LowLevel.Core.CXAvailabilityKind'.
  | ParseUnknownCursorAvailability (SimpleEnum CXAvailabilityKind)

    -- | We encountered an unexpected type kind
    --
    -- This is always a bug in hs-bindgen: if this kind of type is unsupported,
    -- we should explicitly check for it and throw an appropriate exception.
    --
    -- If this is a @Left@ value, it means that our @libclang@ bindings are
    -- incomplete.
  | ParseUnexpectedTypeKind (Either CInt CXTypeKind)

    -- | We encountered an unexpected cursor kind
    --
    -- Similar comments apply as for 'UnexpectedTypeKind'.
  | ParseUnexpectedCursorKind (Either CInt CXCursorKind)

    -- | Complex types can only be defined using primitive types, e.g.
    -- @double complex@. @struct Point complex@ is not allowed.
  | ParseUnexpectedComplexType CXType

  | ParseNoMainHeadersException String SourcePath

  | ParseExpectedFunctionType String

  | ParseUnexpectedLinkage (Either CInt CXLinkageKind)

  | ParseUnexpectedVisibility (Either CInt CXLinkageKind)
  deriving stock (Show, Eq, Ord, Generic)

instance PrettyForTrace ImmediateParseMsg where
  prettyForTrace = \case
      ParseUnexpectedUnderlyingType name err ->
        unexpected $ PP.hcat [
            "underlying type of typedef "
          , prettyForTrace name
          , ": "
          , prettyForTrace err
          ]
      ParseOfDeclarationRequiredForScopingFailed err ->
        PP.hang "Parse of declaration required for scoping failed:" 2 $
          prettyForTrace err
      ParseOfDeclarationMaybeRequiredForScopingFailed err ->
        PP.hang "Parse of declaration maybe required for scoping failed:" 2 $
          prettyForTrace err
      ParseUnknownCursorAvailability simpleKind -> PP.hsep [
          "Unknown declaration cursor availability:"
        , PP.show simpleKind
        ]
      ParseUnexpectedTypeKind x ->
        unexpected $ "type kind " >< either PP.show PP.show x
      ParseUnexpectedCursorKind x ->
        unexpected $ "cursor kind " >< either PP.show PP.show x
      ParseUnexpectedComplexType ty ->
        unexpected $ "complex type " >< PP.show ty
      ParseNoMainHeadersException why whr -> PP.hsep [
          "Could not determine main headers:"
        , PP.string why
        , "at"
        , PP.string $ getSourcePath whr
        ]
      ParseExpectedFunctionType ty -> PP.hsep [
          "Expected function type, but got"
        , PP.string ty
        ]
      ParseUnexpectedLinkage linkage -> PP.hcat [
          "Unexpected linkage: "
        , PP.show linkage
        ]
      ParseUnexpectedVisibility visibility -> PP.hcat [
          "Unexpected visibility: "
        , PP.show visibility
        ]
    where
      unexpected :: PP.CtxDoc -> PP.CtxDoc
      unexpected msg = PP.vcat [
            "Unexpected " >< msg
          , PP.string pleaseReport
          ]

instance IsTrace Level ImmediateParseMsg where
  getDefaultLogLevel = \case
      ParseUnexpectedUnderlyingType _ x                 -> getDefaultLogLevel x
      ParseOfDeclarationRequiredForScopingFailed{}      -> Info
      ParseOfDeclarationMaybeRequiredForScopingFailed{} -> Info
      ParseUnknownCursorAvailability{}                  -> Notice
      ParseUnexpectedTypeKind{}                         -> Warning
      ParseUnexpectedCursorKind{}                       -> Warning
      ParseUnexpectedComplexType{}                      -> Warning
      ParseNoMainHeadersException{}                     -> Error
      ParseExpectedFunctionType{}                       -> Warning
      ParseUnexpectedLinkage{}                          -> Warning
      ParseUnexpectedVisibility{}                       -> Warning
  getSource  = const HsBindgen
  getTraceId = const "parse-immediate"

{-------------------------------------------------------------------------------
  Delayed parse messages
-------------------------------------------------------------------------------}

-- | Delayed parse messages
--
-- We emit these parse messages only when we attempt to select the attached
-- declaration.
--
-- We distinguish between \"unsupported\", which refers to C features that one
-- could reasonably expect to be supported eventually, and \"unexpected\", for C
-- input we are not prepared for.
data DelayedParseMsg =
    -- TODO-D: Merge with ParseUnexpectedUnderlyingType; the severity is
    -- determined by the recursive case.

    -- | Recursive case; we failed to parse the target of a @typedef@ with a
    --   delayed parse message.
    ParseUnsupportedUnderlyingType PrelimDeclId DelayedParseMsg

    -- | Struct with implicit fields
  | ParseUnsupportedImplicitFields

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

    -- | Unusable anonymous declaration
    --
    -- When an unusable declaration appears in some outer declaration (say a
    -- function signature), we will fail to \"parse\" that outer declaration
    -- (actually this happens as a separate post-processing step in
    -- @AssignAnonIds@). We record the identifier of the anonymous declaration
    -- here (that is, it's source location); the identifier of the outer
    -- declaration is recorded in the encloding 'ParseResult'.
  | ParseUnusableAnonDecl AnonId

    -- | We do not support variadic (varargs) functions
  | ParseUnsupportedVariadicFunction

    -- | We do not support @long double@
  | ParseUnsupportedLongDouble

    -- | We do not support @float128@
  | ParseUnsupportedFloat128

    -- | Clang built-in declaration
  | ParseUnsupportedBuiltin Text

  | ParseInvalidLinkage

  | ParseUnsupportedLinkage String CXLinkageKind

  | ParseInvalidVisibility
  deriving stock (Show, Eq, Ord, Generic)

instance PrettyForTrace DelayedParseMsg where
  prettyForTrace = \case
      ParseUnsupportedUnderlyingType name err -> PP.hcat [
          "Unsupported underlying type of typedef "
        , prettyForTrace name
        , ": "
        , prettyForTrace err
        ]
      ParseUnsupportedImplicitFields ->
        "Unsupported implicit fields"
      ParseUnsupportedAnonInSignature ->
        "Unexpected anonymous declaration in function signature"
      ParseUnsupportedAnonInExtern ->
        "Unexpected anonymous declaration in global variable"
      ParseUnsupportedTLS ->
        "Unsupported thread-local variable"
      ParseUnknownStorageClass storage -> PP.hsep [
          "Unsupported storage class"
        , PP.show storage
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
      ParseFunctionOfTypeTypedef ->
        "Unsupported function declared with a typedef type"
      ParseUnusableAnonDecl anonId -> PP.hsep [
          "Unusable anonymous declaration "
        , prettyForTrace anonId
        ]
      ParseUnsupportedVariadicFunction ->
        "Unsupported variadic (varargs) function"
      ParseUnsupportedLongDouble ->
        "Unsupported long double"
      ParseUnsupportedFloat128 ->
        "Unsupported float128"
      ParseUnsupportedBuiltin name ->
        "Unsupported built-in " >< PP.show name
      ParseInvalidLinkage ->
        "Invalid linkage (CXLinkage_Invalid)"
      ParseUnsupportedLinkage comment linkage -> PP.hcat [
          "Unsupported linkage: "
        , PP.show linkage
        , " ("
        , PP.string comment
        , ")"
        ]
      ParseInvalidVisibility ->
        "Invalid visibility (CXVisibility_Invalid)"

-- | Unsupported features are warnings
instance IsTrace Level DelayedParseMsg where
  getDefaultLogLevel = \case
      ParseUnsupportedUnderlyingType{}    -> Warning
      ParseUnsupportedImplicitFields{}    -> Warning
      ParseUnsupportedAnonInSignature{}   -> Warning
      ParseUnsupportedAnonInExtern{}      -> Warning
      ParseUnsupportedTLS{}               -> Warning
      ParseUnknownStorageClass{}          -> Warning
      ParsePotentialDuplicateSymbol{}     -> Notice
      ParseNonPublicVisibility{}          -> Warning
      ParseFunctionOfTypeTypedef{}        -> Warning
      ParseUnusableAnonDecl{}             -> Warning
      ParseUnsupportedVariadicFunction    -> Warning
      ParseUnsupportedLongDouble          -> Warning
      ParseUnsupportedFloat128            -> Warning
      ParseUnsupportedBuiltin{}           -> Warning
      ParseInvalidLinkage                 -> Warning
      ParseUnsupportedLinkage{}           -> Warning
      ParseInvalidVisibility              -> Warning
  getSource  = const HsBindgen
  getTraceId = const "parse"
