-- | Parse messages
module HsBindgen.Frontend.Pass.Parse.Msg (
    ParseTypeException(..)
  , DelayedParseMsg(..)
  ) where

import Control.Exception
import Foreign.C
import Text.SimplePrettyPrint (CtxDoc, (><))
import Text.SimplePrettyPrint qualified as PP

import Clang.Enum.Simple
import Clang.LowLevel.Core

import HsBindgen.Errors
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (AnonId, PrelimDeclId)
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data ParseTypeException =
    -- | We encountered an unexpected type kind
    --
    -- This is always a bug in hs-bindgen: if this kind of type is unsupported,
    -- we should explicitly check for it and throw an appropriate exception.
    --
    -- If this is a @Left@ value, it means that our @libclang@ bindings are
    -- incomplete.
    UnexpectedTypeKind (Either CInt CXTypeKind)

    -- | We encountered an unexpected type declaration
    --
    -- Similar comments apply as for 'UnexpectedTypeKind'.
  | UnexpectedTypeDecl (Either CInt CXCursorKind)

    -- | We do not support variadic (varargs) functions
  | UnsupportedVariadicFunction

    -- | We do not support @long double@
  | UnsupportedLongDouble

    -- | Clang built-in declaration
  | UnsupportedBuiltin Text

    -- | Complex types can only be defined using primitive types, e.g.
    -- @double complex@. @struct Point complex@ is not allowed.
  | UnexpectedComplexType CXType

  | UnsupportedUnderlyingType PrelimDeclId ParseTypeException
  deriving stock (Show, Eq, Ord)

instance Exception ParseTypeException where
  displayException = PP.renderCtxDoc (PP.mkContext 100) . prettyForTrace

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

    -- | Unusable anonymous declaration
    --
    -- When an unusable declaration appears in some outer declaration (say a
    -- function signature), we will fail to \"parse\" that outer declaration
    -- (actually this happens as a separate post-processing step in
    -- @AssignAnonIds@). We record the identifier of the anonymous declaration
    -- here (that is, it's source location); the identifier of the outer
    -- declaration is recorded in the encloding 'ParseResult'.
  | ParseUnusableAnonDecl AnonId
  deriving stock (Show, Eq, Ord, Generic)

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance PrettyForTrace ParseTypeException where
  prettyForTrace = \case
      UnexpectedTypeKind (Right kind) ->
          unexpected $ "type kind " >< PP.show kind
      UnexpectedTypeKind (Left i) ->
          unexpected $ "type kind " >< PP.show i
      UnexpectedTypeDecl (Right kind) ->
          unexpected $ "type declaration " >< PP.show kind
      UnexpectedTypeDecl (Left i) ->
          unexpected $ "type declaration " >< PP.show i
      UnsupportedVariadicFunction ->
          "Unsupported variadic (varargs) function"
      UnsupportedLongDouble ->
          "Unsupported long double"
      UnsupportedBuiltin name ->
          "Unsupported built-in " >< PP.show name
      UnexpectedComplexType ty ->
          "Unexpected complex type " >< PP.show ty
      UnsupportedUnderlyingType name err -> PP.hcat [
            "Unsupported underlying type of typedef "
          , PP.show name
          , ": "
          , prettyForTrace err
          ]
    where
      unexpected :: PP.CtxDoc -> PP.CtxDoc
      unexpected msg = PP.vcat [
            "Unexpected " >< msg
          , PP.string pleaseReport
          ]

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
      ParseFunctionOfTypeTypedef -> noBindingsGenerated $
        "unsupported function declared with a typedef type"
      ParseUnusableAnonDecl anonId -> noBindingsGenerated $ PP.hsep [
          "unusable anonymous declaration "
        , prettyForTrace anonId
        ]
    where
      noBindingsGenerated :: CtxDoc -> CtxDoc
      noBindingsGenerated reason = PP.hsep ["No bindings generated:", reason]

{-------------------------------------------------------------------------------
  Severity
-------------------------------------------------------------------------------}

-- | We use 'Error' for bugs, and 'Warning' for known-to-be-unsupported
--
-- This ensures that for declarations that use known-to-be-unsupported types, we
-- can just register a parse failure and avoid generating bindings for that
-- declaration.
instance IsTrace Level ParseTypeException where
  getDefaultLogLevel = \case
    UnexpectedTypeKind{}          -> Error
    UnexpectedTypeDecl{}          -> Error
    UnsupportedVariadicFunction   -> Warning
    UnsupportedLongDouble         -> Warning
    UnsupportedBuiltin{}          -> Warning
    UnexpectedComplexType{}       -> Error
    UnsupportedUnderlyingType _ x -> getDefaultLogLevel x
  getSource  = const HsBindgen
  getTraceId = const "parse-type-exception"

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
      ParseUnusableAnonDecl{}          -> Warning
  getSource  = const HsBindgen
  getTraceId = const "parse-delayed"
