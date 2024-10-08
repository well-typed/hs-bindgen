-- | Low-level bindings to @libclang@
--
-- The goal of these bindings is to provide an API which is as close as possible
-- to using the C API, whilst taking care of the most annoying low-level details
-- such as
--
-- * callbacks
-- * bytestrings
-- * out-parameters
-- * checking results for errors and throwing exceptions
-- * etc.
--
-- Despite the low-level nature, these bindings should ideally be useable
-- without imports of @Foreign.*@.
--
-- Guidelines:
--
-- * The goal of this module is not to be a complete set of bindings for all of
--   @libclang@, but rather only to the parts that we need. We do include
--   documentation.
--
-- * Structs are left opaque, with provided accessors where necessary. An
--   accessor for a field @field@ of a type @CXFooBar@ is called @cxfbField@.
--
-- * For functions that take structs or return structs by value, we use our own
--   wrappers from @cbits/clang_wrappers.h@, along with the infrastructure in
--   "HsBindgen.Clang.Internal.ByValue".
--
-- Most sections in this module and in the export list correspond to
-- <https://clang.llvm.org/doxygen/group__CINDEX.html>; see also
-- <https://clang.llvm.org/doxygen/modules.html> for the full list.
--
-- /Note on naming/: When exposing a @libclang@ function called @clang_foo@, we
-- will call the corresponding Haskell function also @clang_foo@, so that the
-- Haskell API is as close to the C API as possible. However, the Haskell
-- function @clang_foo@ does (usually) not bind /directly/ to the C function:
--
-- 1. The majority of functions we don't import from @libclang@ directly, but
--    instead from our custom C wrappers. These C wrapper functions are called
--    @wrap_foo@, and are imported as such.
--
-- 2. For functions for which we don't need a C wrapper, we import the
--    @libclang@ function as @nowrapper_foo@.
--
-- 3. In the rare case that we can export the C function directly, we import it
--    simply as @clang_foo@.
--
-- /Note on pointers/: in the public API, all @libclang@ types are opaque.
-- Internally, they are all newtypes around either 'Ptr' or 'OnHaskellHeap',
-- depending on whether or not we own the value.
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/80> Ideally we would
-- bootstrap this (generate it using @hs-bindgen@ itself).
module HsBindgen.Clang.Core (
    -- * Top-level
    CXIndex
  , DisplayDiagnostics(..)
  , clang_createIndex
  , clang_getNumDiagnostics
  , clang_getDiagnostic
    -- * Diagnostic reporting
  , CXDiagnostic
  , CXDiagnosticSet
  , CXDiagnosticDisplayOptions(..)
  , CXDiagnosticSeverity(..)
  , clang_getNumDiagnosticsInSet
  , clang_getDiagnosticInSet
  , clang_disposeDiagnosticSet
  , clang_getChildDiagnostics
  , clang_disposeDiagnostic
  , clang_formatDiagnostic
  , clang_defaultDiagnosticDisplayOptions
  , clang_getDiagnosticSeverity
  , clang_getDiagnosticLocation
  , clang_getDiagnosticSpelling
  , clang_getDiagnosticOption
  , clang_getDiagnosticCategory
  , clang_getDiagnosticCategoryText
  , clang_getDiagnosticNumRanges
  , clang_getDiagnosticRange
  , clang_getDiagnosticNumFixIts
  , clang_getDiagnosticFixIt
    -- * Translation unit manipulation
  , CXTranslationUnit
  , CXUnsavedFile
  , CXTranslationUnit_Flags(..)
  , CXTargetInfo(..)
  , clang_parseTranslationUnit
  , clang_getTranslationUnitTargetInfo
  , clang_TargetInfo_dispose
  , clang_TargetInfo_getTriple
    -- * Cursor manipulations
  , CXCursor
  , CXCursorKind(..)
  , clang_getTranslationUnitCursor
  , clang_equalCursors
  , clang_getCursorSemanticParent
  , clang_getCursorLexicalParent
  , clang_getCursorKind
  , clang_getCursorKindSpelling
  , clang_Cursor_getTranslationUnit
  , clang_isDeclaration
    -- * Traversing the AST with cursors
  , CXChildVisitResult(..)
  , clang_visitChildren
    -- * Cross-referencing in the AST
  , clang_getCursorDisplayName
  , clang_getCursorSpelling
  , clang_getCursorReferenced
  , clang_getCursorDefinition
  , clang_getCanonicalCursor
  , clang_Cursor_getRawCommentText
  , clang_Cursor_getBriefCommentText
  , clang_Cursor_getSpellingNameRange
  , clang_isCursorDefinition
    -- * Type information for CXCursors
  , CXTypeKind(..)
  , CXType
  , cxtKind
  , clang_getCursorType
  , clang_getTypeKindSpelling
  , clang_getTypeSpelling
  , clang_getTypedefDeclUnderlyingType
  , clang_getPointeeType
  , clang_Type_getSizeOf
  , clang_Type_getAlignOf
  , clang_Type_isTransparentTagTypedef
  , clang_Cursor_getOffsetOfField
  , clang_Cursor_isAnonymous
  , clang_getEnumConstantDeclValue
  , clang_getCanonicalType
  , clang_getTypedefName
  , clang_getUnqualifiedType
  , clang_getTypeDeclaration
  , clang_Type_getNamedType
  , clang_Type_getModifiedType
  , clang_Type_getValueType
    -- * Mapping between cursors and source code
  , CXSourceRange
  , clang_getCursorLocation
  , clang_getCursorExtent
    -- * Token extraction and manipulation
  , CXToken
  , CXTokenKind(..)
  , clang_getToken
  , clang_getTokenKind
  , clang_getTokenSpelling
  , clang_getTokenLocation
  , clang_getTokenExtent
  , clang_tokenize
  , clang_disposeTokens
  , index_CXTokenArray
  , clang_annotateTokens
  , index_CXCursorArray
    -- * Physical source locations
  , CXSourceLocation
  , CXFile
  , clang_getRangeStart
  , clang_getRangeEnd
  , clang_getExpansionLocation
  , clang_getPresumedLocation
  , clang_getSpellingLocation
  , clang_getFileLocation
  , clang_getLocation
  , clang_getRange
  , clang_getFile
  , clang_Location_isFromMainFile
    -- * File manipulation routines
  , clang_getFileName
    -- * Debugging
  , clang_breakpoint
    -- * Exceptions
  , CallFailed(..)
  ) where

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as Text
import Foreign
import Foreign.C
import GHC.Stack
import System.IO.Unsafe (unsafePerformIO)

import HsBindgen.Clang.Args
import HsBindgen.Clang.Core.Enums
import HsBindgen.Clang.Core.Instances ()
import HsBindgen.Clang.Core.Structs
import HsBindgen.Clang.Internal.ByValue
import HsBindgen.Clang.Internal.CXString ()
import HsBindgen.Clang.Internal.FFI
import HsBindgen.Clang.Internal.Results
import HsBindgen.Clang.Version
import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Top-level

  <https://clang.llvm.org/doxygen/group__CINDEX.html>
-------------------------------------------------------------------------------}

-- | An "index" that consists of a set of translation units that would typically
-- be linked together into an executable or library.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#gae039c2574bfd75774ca7a9a3e55910cb>
newtype CXIndex = CXIndex (Ptr ())
  deriving stock (Show)

data DisplayDiagnostics =
    DisplayDiagnostics
  | DontDisplayDiagnostics

foreign import capi unsafe "clang-c/Index.h clang_createIndex"
  nowrapper_clang_createIndex ::
       CInt -- ^ @excludeDeclarationsFromPCH@
    -> CInt -- ^ @displayDiagnostics@
    -> IO CXIndex

-- | Determine the number of diagnostics produced for the given translation unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#gae9f047b4bbbbb01161478d549b7aab25>
foreign import capi unsafe "clang-c/Index.h clang_getNumDiagnostics"
  clang_getNumDiagnostics :: CXTranslationUnit -> IO CUInt

-- | Retrieve a diagnostic associated with the given translation unit.
--
-- Returns the requested diagnostic. This diagnostic must be freed via a call to
-- 'clang_disposeDiagnostic'.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#ga3f54a79e820c2ac9388611e98029afe5>
foreign import capi unsafe "clang-c/Index.h clang_getDiagnostic"
  clang_getDiagnostic ::
       CXTranslationUnit
       -- ^ the translation unit to query.
    -> CUInt
       -- ^ the zero-based diagnostic number to retrieve.
    -> IO CXDiagnostic

-- | Provides a shared context for creating translation units.
--
-- /NOTE/: We are not planning to support precompiled headers, so we omit the
-- first argument (@excludeDeclarationsFromPCH@).
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#ga51eb9b38c18743bf2d824c6230e61f93>
clang_createIndex ::
     DisplayDiagnostics
  -> IO CXIndex
clang_createIndex diagnostics =
    nowrapper_clang_createIndex 0 diagnostics'
  where
    diagnostics' :: CInt
    diagnostics' =
        case diagnostics of
          DisplayDiagnostics     -> 1
          DontDisplayDiagnostics -> 0

{-------------------------------------------------------------------------------
  Diagnostic reporting

  <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html>
-------------------------------------------------------------------------------}

-- | A single diagnostic, containing the diagnostic's severity, location, text,
-- source ranges, and fix-it hints.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga44bb8aba7c40590ad25d1763c4fbff7f>
newtype CXDiagnostic = CXDiagnostic (Ptr ())

-- | A group of CXDiagnostics.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga38dfc0ae45b55bf7fd577eed9148e244>
newtype CXDiagnosticSet = CXDiagnosticSet (Ptr ())

-- | Determine the number of diagnostics in a 'CXDiagnosticSet'.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga44e87e54125e501de0d3bd29161fe26b>
foreign import capi unsafe "clang-c/Index.h clang_getNumDiagnosticsInSet"
  clang_getNumDiagnosticsInSet :: CXDiagnosticSet -> IO CUInt

-- | Retrieve a diagnostic associated with the given 'CXDiagnosticSet'.
--
-- Returns the requested diagnostic. This diagnostic must be freed via a call to
-- 'clang_disposeDiagnostic'.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga997e07d587e02eea7d29874c33c94249>
foreign import capi unsafe "clang-c/Index.h clang_getDiagnosticInSet"
  clang_getDiagnosticInSet ::
       CXDiagnosticSet  -- ^ the CXDiagnosticSet to query.
    -> CUInt            -- ^ the zero-based diagnostic number to retrieve.
    -> IO CXDiagnostic

-- | Release a CXDiagnosticSet and all of its contained diagnostics.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga1a1126b07e4dc0b45b0617f3cc848d57>
foreign import capi unsafe "clang-c/Index.h clang_disposeDiagnosticSet"
  clang_disposeDiagnosticSet :: CXDiagnosticSet -> IO ()

-- | Retrieve the child diagnostics of a CXDiagnostic.
--
-- This 'CXDiagnosticSet' does not need to be released by
-- 'clang_disposeDiagnosticSet'.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga1aa24f925b34bb988dc3ea06ec27dcda>
foreign import capi unsafe "clang-c/Index.h clang_getChildDiagnostics"
  clang_getChildDiagnostics :: CXDiagnostic -> IO CXDiagnosticSet

-- | Destroy a diagnostic.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga07061e0ad7665b7c5ee7253cd1bf4a5c>
foreign import capi unsafe "clang-c/Index.h clang_disposeDiagnostic"
  clang_disposeDiagnostic :: CXDiagnostic -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_formatDiagnostic"
  wrap_formatDiagnostic ::
       CXDiagnostic
    -> BitfieldEnum CXDiagnosticDisplayOptions
    -> W CXString_
    -> IO ()

-- | Retrieve the set of display options most similar to the default behavior of
-- the clang compiler.
--
-- Returns a set of display options suitable for use with
-- 'clang_formatDiagnostic'.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga5fcf910792541399efd63c62042ce353>
foreign import capi unsafe "clang-c/Index.h clang_defaultDiagnosticDisplayOptions"
  clang_defaultDiagnosticDisplayOptions ::
       IO (BitfieldEnum CXDiagnosticDisplayOptions)

-- | Determine the severity of the given diagnostic.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#gaff14261578eb9a2b02084f0cc6b95f9a>
foreign import capi unsafe "clang-c/Index.h clang_getDiagnosticSeverity"
  clang_getDiagnosticSeverity ::
       CXDiagnostic
    -> IO (SimpleEnum CXDiagnosticSeverity)

foreign import capi unsafe "clang_wrappers.h wrap_getDiagnosticLocation"
  wrap_getDiagnosticLocation :: CXDiagnostic -> W CXSourceLocation_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getDiagnosticSpelling"
  wrap_getDiagnosticSpelling :: CXDiagnostic -> W CXString_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getDiagnosticOption"
  wrap_getDiagnosticOption ::
        CXDiagnostic
     -> W CXString_
     -> W CXString_
     -> IO ()

-- | Retrieve the category number for this diagnostic.
--
-- Diagnostics can be categorized into groups along with other, related
-- diagnostics (e.g., diagnostics under the same warning flag). This routine
-- retrieves the category number for the given diagnostic.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga0ec085bd59b8b6c935eab0e53a1f348f>
foreign import capi unsafe "clang-c/Index.h clang_getDiagnosticCategory"
  clang_getDiagnosticCategory :: CXDiagnostic -> IO CUInt

foreign import capi unsafe "clang_wrappers.h wrap_getDiagnosticCategoryText"
  wrap_getDiagnosticCategoryText :: CXDiagnostic -> W CXString_ -> IO ()

-- | Determine the number of source ranges associated with the given diagnostic.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga7acbd761f1113ea657022e5708694924>
foreign import capi unsafe "clang-c/Index.h clang_getDiagnosticNumRanges"
  clang_getDiagnosticNumRanges :: CXDiagnostic -> IO CUInt

foreign import capi unsafe "clang_wrappers.h wrap_getDiagnosticRange"
  wrap_getDiagnosticRange :: CXDiagnostic -> CUInt -> W CXSourceRange_ -> IO ()

-- | Determine the number of fix-it hints associated with the given diagnostic.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#gafe38dfd661f6ba59df956dfeabece2a2>
foreign import capi unsafe "clang-c/Index.h clang_getDiagnosticNumFixIts"
  clang_getDiagnosticNumFixIts :: CXDiagnostic -> IO CUInt

foreign import capi unsafe "clang_wrappers.h wrap_getDiagnosticFixIt"
  wrap_getDiagnosticFixIt ::
       CXDiagnostic
    -> CUInt
    -> W CXSourceRange_
    -> W CXString_
    -> IO ()

-- | Format the given diagnostic in a manner that is suitable for display.
--
-- This routine will format the given diagnostic to a string, rendering the
-- diagnostic according to the various options given. The
-- 'clang_defaultDiagnosticDisplayOptions' function returns the set of options
-- that most closely mimics the behavior of the clang compiler.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga455234ab6de0ca12c9ea36f8874060e8>
clang_formatDiagnostic ::
     CXDiagnostic
  -> BitfieldEnum CXDiagnosticDisplayOptions
  -> IO Text
clang_formatDiagnostic diagnostic options =
    preallocate_ $ wrap_formatDiagnostic diagnostic options

-- | Retrieve the source location of the given diagnostic.
--
-- This location is where Clang would print the caret (@^@) when displaying the
-- diagnostic on the command line.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#gabfcf70ac15bb3e5ae39ef2c5e07c7428>
clang_getDiagnosticLocation :: CXDiagnostic -> IO CXSourceLocation
clang_getDiagnosticLocation diagnostic =
    preallocate_ $ wrap_getDiagnosticLocation diagnostic

-- | Retrieve the text of the given diagnostic.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga34a875e6d06ed4f8d2fc032f850ebbe1>
clang_getDiagnosticSpelling :: CXDiagnostic -> IO Text
clang_getDiagnosticSpelling diagnostic =
    preallocate_ $ wrap_getDiagnosticSpelling diagnostic

-- | Retrieve the name of the command-line option that enabled this diagnostic.
--
-- Returns a string that contains the command-line option used to enable this
-- warning, such as @"-Wconversion"@ or @"-pedantic"@, as well as the option
-- that disables this diagnostic (if any).
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga69b094e2cca1cd6f452327dc9204a168>
clang_getDiagnosticOption :: CXDiagnostic -> IO (Text, Text)
clang_getDiagnosticOption diagnostic =
    preallocatePair_ $ wrap_getDiagnosticOption diagnostic

-- | Retrieve the diagnostic category text for a given diagnostic.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga6950702b6122f1cd74e1a369605a9f54>>
clang_getDiagnosticCategoryText :: CXDiagnostic -> IO Text
clang_getDiagnosticCategoryText diagnostic =
    preallocate_ $ wrap_getDiagnosticCategoryText diagnostic

-- | Retrieve a source range associated with the diagnostic.
--
-- A diagnostic's source ranges highlight important elements in the source code.
-- On the command line, Clang displays source ranges by underlining them with
-- @~@ characters.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#gabd440f1577374289ffebe73d9f65b294>
clang_getDiagnosticRange ::
     CXDiagnostic  -- ^ the diagnostic whose range is being extracted.
  -> CUInt         -- ^ the zero-based index specifying which range to extract
  -> IO CXSourceRange
clang_getDiagnosticRange diagnostic range =
    preallocate_ $ wrap_getDiagnosticRange diagnostic range

-- | Retrieve the replacement information for a given fix-it.
--
-- Fix-its are described in terms of a source range whose contents should be
-- replaced by a string. This approach generalizes over three kinds of
-- operations: removal of source code (the range covers the code to be removed
-- and the replacement string is empty), replacement of source code (the range
-- covers the code to be replaced and the replacement string provides the new
-- code), and insertion (both the start and end of the range point at the
-- insertion location, and the replacement string provides the text to insert).
--
-- Returns the replacement range and a string containing text that should be
-- replace the source code. The replacement range is the source range whose
-- contents will be replaced with the returned replacement string. Note that
-- source ranges are half-open ranges [a, b), so the source code should be
-- replaced from a and up to (but not including) b.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#gadf990bd68112475c5c07b19c1fe3938a>
clang_getDiagnosticFixIt ::
     CXDiagnostic  -- ^ The diagnostic whose fix-its are being queried.
  -> CUInt         -- ^ The zero-based index of the fix-it.
  -> IO (CXSourceRange, Text)
clang_getDiagnosticFixIt diagnostic fixit =
    preallocatePair_ $ wrap_getDiagnosticFixIt diagnostic fixit

{-------------------------------------------------------------------------------
  Translation unit manipulation

  <https://clang.llvm.org/doxygen/group__CINDEX__TRANSLATION__UNIT.html>
-------------------------------------------------------------------------------}

-- | A single translation unit, which resides in an index.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#gacdb7815736ca709ce9a5e1ec2b7e16ac>
newtype CXTranslationUnit = CXTranslationUnit (Ptr ())
  deriving stock (Show)

-- | Provides the contents of a file that has not yet been saved to disk.
--
-- Each 'CXUnsavedFile' instance provides the name of a file on the system along
-- with the current contents of that file that have not yet been saved to disk.
--
-- We don't export this type.
--
-- <https://clang.llvm.org/doxygen/structCXUnsavedFile.html>
type CXUnsavedFile = Ptr ()

-- | An opaque type representing target information for a given translation
-- unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#ga6b47552ab8c5d81387070a9b197cd3e2>
newtype {-# CType "CXTargetInfo" #-} CXTargetInfo = CXTargetInfo (Ptr ())
  deriving stock (Show)

-- We use @ccall@ rather than @capi@ to avoid problems with the
-- @const char *const *@ type
-- (see also <https://gitlab.haskell.org/ghc/ghc/-/issues/22043>).
foreign import ccall unsafe "clang-c/Index.h clang_parseTranslationUnit"
  nowrapper_parseTranslationUnit ::
       CXIndex
    -> CString
    -> Ptr CString
    -> CInt
    -> CXUnsavedFile
    -> CUInt
    -> BitfieldEnum CXTranslationUnit_Flags
    -> IO CXTranslationUnit

-- | Get target information for this translation unit.
--
-- The 'CXTargetInfo' object cannot outlive the 'CXTranslationUnit' object.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TRANSLATION__UNIT.html#ga1813b53c06775c354f4797a5ec051948>
foreign import capi "clang_wrappers.h clang_getTranslationUnitTargetInfo"
  clang_getTranslationUnitTargetInfo :: CXTranslationUnit -> IO CXTargetInfo

-- | Destroy the 'CXTargetInfo' object.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TRANSLATION__UNIT.html#gafb00d82420b0101c185b88338567ffd9>
foreign import capi "clang_wrappers.h clang_TargetInfo_dispose"
  clang_TargetInfo_dispose :: CXTargetInfo -> IO ()

foreign import capi "clang_wrappers.h wrap_TargetInfo_getTriple"
  wrap_TargetInfo_getTriple :: CXTargetInfo -> W CXString_ -> IO ()

-- | Same as 'clang_parseTranslationUnit2', but returns the 'CXTranslationUnit'
-- instead of an error code.
--
-- Throws 'CallFailed' in case of an error.
--
-- /NOTE/: We omit the argument for unsaved files; we have no plans for
-- supporting a workflow where the C headers are being edited, and the bindings
-- dynamically updated.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TRANSLATION__UNIT.html#ga2baf83f8c3299788234c8bce55e4472e>
clang_parseTranslationUnit ::
     HasCallStack
  => CXIndex                               -- ^ @CIdx@
  -> FilePath                              -- ^ @source_filename@
  -> ClangArgs                             -- ^ @command_line_args@
  -> BitfieldEnum CXTranslationUnit_Flags  -- ^ @options@
  -> IO CXTranslationUnit
clang_parseTranslationUnit cIdx src args options =
    case fromClangArgs args of
      Left  err   -> callFailed err
      Right args' ->
        withCString  src   $ \src' ->
        withCStrings args' $ \args'' numArgs -> ensureNotNull $
          nowrapper_parseTranslationUnit
            cIdx
            src'
            args''
            numArgs
            nullPtr
            0
            options

-- | Get the normalized target triple as a string.
--
-- Throws 'CallFailed' on error.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TRANSLATION__UNIT.html#ga7ae67e3c8baf6a9852900f6529dce2d0>
clang_TargetInfo_getTriple :: HasCallStack => CXTargetInfo -> IO Text
clang_TargetInfo_getTriple info = ensure (not . Text.null) $
    preallocate_ $ wrap_TargetInfo_getTriple info

{-------------------------------------------------------------------------------
  Cursor manipulations

  <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html>
-------------------------------------------------------------------------------}

-- | A cursor representing some element in the abstract syntax tree for a
-- translation unit.
--
-- The cursor abstraction unifies the different kinds of entities in a
-- program–declaration, statements, expressions, references to declarations,
-- etc.–under a single "cursor" abstraction with a common set of operations.
-- Common operation for a cursor include: getting the physical location in a
-- source file where the cursor points, getting the name associated with a
-- cursor, and retrieving cursors for any child nodes of a particular cursor.
--
-- Cursors can be produced in two specific ways:
--
-- * 'clang_getTranslationUnitCursor' produces a cursor for a translation unit,
--   from which one can use 'clang_visitChildren' to explore the rest of the
--   translation unit.
-- * 'clang_getCursor' maps from a physical source location to the entity that
--   resides at that location, allowing one to map from the source code into the
--   AST.
--
-- <https://clang.llvm.org/doxygen/structCXCursor.html>
newtype CXCursor = CXCursor (OnHaskellHeap CXCursor_)
  deriving newtype (LivesOnHaskellHeap, Preallocate)

foreign import capi unsafe "clang_wrappers.h wrap_getTranslationUnitCursor"
  wrap_getTranslationUnitCursor :: CXTranslationUnit -> W CXCursor_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_equalCursors"
  wrap_equalCursors :: R CXCursor_ -> R CXCursor_ -> IO CUInt

foreign import capi unsafe "clang_wrappers.h wrap_getCursorSemanticParent"
  wrap_getCursorSemanticParent :: R CXCursor_ -> W CXCursor_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getCursorLexicalParent"
  wrap_getCursorLexicalParent :: R CXCursor_ -> W CXCursor_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getCursorKind"
  wrap_getCursorKind :: R CXCursor_ -> IO (SimpleEnum CXCursorKind)

foreign import capi unsafe "clang_wrappers.h wrap_getCursorKindSpelling"
  wrap_getCursorKindSpelling :: SimpleEnum CXCursorKind -> W CXString_ -> IO ()

foreign import capi unsafe "wrap_Cursor_getTranslationUnit"
  wrap_Cursor_getTranslationUnit :: R CXCursor_ -> IO CXTranslationUnit

foreign import capi unsafe "clang-c/Index.h clang_isDeclaration"
  nowrapper_isDeclaration :: SimpleEnum CXCursorKind -> IO CUInt

-- | Retrieve the cursor that represents the given translation unit.
--
-- The translation unit cursor can be used to start traversing the various
-- declarations within the given translation unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#gaec6e69127920785e74e4a517423f4391>
clang_getTranslationUnitCursor :: CXTranslationUnit -> IO CXCursor
clang_getTranslationUnitCursor unit =
    preallocate_ $ wrap_getTranslationUnitCursor unit

-- | Determine whether two cursors are equivalent.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#ga98df58f09878710b983b6f3f60f0cba3>
clang_equalCursors :: CXCursor -> CXCursor -> IO Bool
clang_equalCursors a b =
    onHaskellHeap a $ \a' ->
    onHaskellHeap b $ \b' ->
      cToBool <$> wrap_equalCursors a' b'

-- | Determine the semantic parent of the given cursor.
--
-- The semantic parent of a cursor is the cursor that semantically contains the
-- given cursor. For many declarations, the lexical and semantic parents are
-- equivalent (the lexical parent is returned by
-- 'clang_getCursorLexicalParent'). They diverge when declarations or
-- definitions are provided out-of-line. For example:
--
-- > class C {
-- >  void f();
-- > };
-- >
-- > void C::f() { }
--
-- In the out-of-line definition of @C::f@, the semantic parent is the class
-- @C@, of which this function is a member. The lexical parent is the place
-- where the declaration actually occurs in the source code; in this case, the
-- definition occurs in the translation unit. In general, the lexical parent for
-- a given entity can change without affecting the semantics of the program, and
-- the lexical parent of different declarations of the same entity may be
-- different. Changing the semantic parent of a declaration, on the other hand,
-- can have a major impact on semantics, and redeclarations of a particular
-- entity should all have the same semantic context.
--
-- In the example above, both declarations of @C::f@ have @C@ as their semantic
-- context, while the lexical context of the first @C::f@ is @C@ and the lexical
-- context of the second @C::f@ is the translation unit.
--
-- For global declarations, the semantic parent is the translation unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#gabc327b200d46781cf30cb84d4af3c877>
clang_getCursorSemanticParent :: CXCursor -> IO CXCursor
clang_getCursorSemanticParent cursor =
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_getCursorSemanticParent cursor'

-- | Determine the lexical parent of the given cursor.
--
-- The lexical parent of a cursor is the cursor in which the given cursor was
-- actually written. For many declarations, the lexical and semantic parents are
-- equivalent (the semantic parent is returned by
-- 'clang_getCursorSemanticParent'). They diverge when declarations or
-- definitions are provided out-of-line. For example:
--
-- > class C {
-- >  void f();
-- > };
-- >
-- > void C::f() { }
--
--
-- In the out-of-line definition of @C::f@, the semantic parent is the class
-- @C@, of which this function is a member. The lexical parent is the place
-- where the declaration actually occurs in the source code; in this case, the
-- definition occurs in the translation unit. In general, the lexical parent for
-- a given entity can change without affecting the semantics of the program, and
-- the lexical parent of different declarations of the same entity may be
-- different. Changing the semantic parent of a declaration, on the other hand,
-- can have a major impact on semantics, and redeclarations of a particular
-- entity should all have the same semantic context.
--
-- In the example above, both declarations of @C::f@ have @C@ as their semantic
-- context, while the lexical context of the first @C::f@ is @C@ and the lexical
-- context of the second @C::f@ is the translation unit.
--
-- For declarations written in the global scope, the lexical parent is the
-- translation unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#gace7a423874d72b3fdc71d6b0f31830dd>
clang_getCursorLexicalParent :: CXCursor -> IO CXCursor
clang_getCursorLexicalParent cursor =
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_getCursorLexicalParent cursor'

-- | Retrieve the kind of the given cursor.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#ga018aaf60362cb751e517d9f8620d490c>
clang_getCursorKind :: CXCursor -> IO (SimpleEnum CXCursorKind)
clang_getCursorKind cursor =
    onHaskellHeap cursor $ \cursor' ->
      wrap_getCursorKind cursor'

-- | Get spelling of a cursor
--
-- NOTE: This is from the @libclang@ \"Debugging facilities\"
-- (https://clang.llvm.org/doxygen/group__CINDEX__DEBUG.html). This should be
-- used only for testing and debugging, and should not be relied upon.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DEBUG.html#ga7a4eecfc1b343568cb9ea447cbde08a8
clang_getCursorKindSpelling :: SimpleEnum CXCursorKind -> IO Text
clang_getCursorKindSpelling kind =
    preallocate_ $ wrap_getCursorKindSpelling kind

-- | Returns the translation unit that a cursor originated from.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#ga529f1504710a41ce358d4e8c3161848d>
clang_Cursor_getTranslationUnit :: CXCursor -> IO (Maybe CXTranslationUnit)
clang_Cursor_getTranslationUnit cursor = checkNotNull $
    onHaskellHeap cursor $ \cursor' ->
      wrap_Cursor_getTranslationUnit cursor'

-- | Determine whether the given cursor kind represents a declaration.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#ga660aa4846fce0a54e20073ab6a5465a0>
clang_isDeclaration :: SimpleEnum CXCursorKind -> IO Bool
clang_isDeclaration kind = cToBool <$> nowrapper_isDeclaration kind

{-------------------------------------------------------------------------------
  Traversing the AST with cursors

  <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__TRAVERSAL.html>
-------------------------------------------------------------------------------}

-- | Visitor invoked for each cursor found by a traversal.
--
-- This is an internal type.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__TRAVERSAL.html#gabf842c9ee20048b596eb9dfe94bb1570>
type WrapCXCursorVisitor =
     Ptr CXCursor_ -- ^ The cursor being visited.
  -> Ptr CXCursor_ -- ^ The parent visitor for that cursor.
  -> IO (SimpleEnum CXChildVisitResult)
     -- ^ The visitor should return one of the 'CXChildVisitResult' values to
     -- direct 'clang_visitCursorChildren'.

foreign import ccall "wrapper"
  mkCursorVisitor :: WrapCXCursorVisitor -> IO (FunPtr WrapCXCursorVisitor)

-- | See 'clang_visitChildren' for docs
--
-- /NOTE/: This is marked @safe@ rather than @unsafe@ as this calls back into
-- Haskell.
foreign import capi safe "clang_wrappers.h wrap_visitChildren"
  wrap_visitChildren :: R CXCursor_ -> FunPtr WrapCXCursorVisitor -> IO CUInt

-- | Visit the children of a particular cursor.
--
-- This function visits all the direct children of the given cursor, invoking
-- the given visitor function with the cursors of each visited child. The
-- traversal may be recursive, if the visitor returns 'CXChildVisit_Recurse'.
-- The traversal may also be ended prematurely, if the visitor returns
-- 'CXChildVisit_Break'.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__TRAVERSAL.html#ga5d0a813d937e1a7dcc35f206ad1f7a91>
clang_visitChildren ::
     CXCursor
     -- ^ @parent@
     --
     -- The cursor whose child may be visited. All kinds of cursors can be
     -- visited, including invalid cursors (which, by definition, have no
     -- children).
  -> (    CXCursor
       -> CXCursor
       -> IO (SimpleEnum CXChildVisitResult)
     )
     -- ^ @visitor@
     --
     -- The visitor function that will be invoked for each child of parent.
     -- It is passed the the cursor being visited and its parent.
     --
     -- /NOTE/: We omit the @client_data@ argument from @libclang@, as it is
     -- not needed in Haskell (the IO action can have arbitrary data in its
     -- closure).
  -> IO Bool
     -- ^ 'True' if the traversal was terminated prematurely by the visitor
     -- returning 'CXChildVisit_Break'.
clang_visitChildren root visitor = do
    visitor' <- mkCursorVisitor $ \current parent -> do
      current' <- CXCursor <$> copyToHaskellHeap current
      parent'  <- CXCursor <$> copyToHaskellHeap parent
      visitor current' parent'
    onHaskellHeap root $ \parent' ->
      (/= 0) <$> wrap_visitChildren parent' visitor'

{-------------------------------------------------------------------------------
  Cross-referencing in the AST

  <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html>
-------------------------------------------------------------------------------}

foreign import capi unsafe "clang_wrappers.h wrap_getCursorDisplayName"
  wrap_getCursorDisplayName :: R CXCursor_ -> W CXString_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getCursorSpelling"
  wrap_getCursorSpelling :: R CXCursor_ -> W CXString_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getCursorReferenced"
  wrap_getCursorReferenced :: R CXCursor_ -> W CXCursor_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getCursorDefinition"
  wrap_getCursorDefinition :: R CXCursor_ -> W CXCursor_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getCanonicalCursor"
  wrap_getCanonicalCursor :: R CXCursor_ -> W CXCursor_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_Cursor_getRawCommentText"
  wrap_Cursor_getRawCommentText :: R CXCursor_ -> W CXString_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_Cursor_getBriefCommentText"
  wrap_Cursor_getBriefCommentText :: R CXCursor_ -> W CXString_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_Cursor_getSpellingNameRange"
  wrap_Cursor_getSpellingNameRange ::
       R CXCursor_
    -> CUInt
    -> CUInt
    -> W CXSourceRange_
    -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_isCursorDefinition"
  wrap_isCursorDefinition :: R CXCursor_ -> IO CUInt

-- | Retrieve the display name for the entity referenced by this cursor.
--
-- The display name contains extra information that helps identify the cursor,
-- such as the parameters of a function or template or the arguments of a class
-- template specialization.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#gac3eba3224d109a956f9ef96fd4fe5c83>
clang_getCursorDisplayName :: CXCursor -> IO Text
clang_getCursorDisplayName cursor =
    onHaskellHeap cursor $ \cursor' ->
      preallocate_$ wrap_getCursorDisplayName cursor'

-- | Retrieve a name for the entity referenced by this cursor.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#gaad1c9b2a1c5ef96cebdbc62f1671c763>
clang_getCursorSpelling :: CXCursor -> IO Text
clang_getCursorSpelling cursor =
    onHaskellHeap cursor $ \cursor' ->
      preallocate_$ wrap_getCursorSpelling cursor'

-- | For a cursor that is a reference, retrieve a cursor representing the entity
-- that it references.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#gabf059155921552e19fc2abed5b4ff73a>
clang_getCursorReferenced :: CXCursor -> IO CXCursor
clang_getCursorReferenced cursor =
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_getCursorReferenced cursor'

-- | For a cursor that is either a reference to or a declaration of some entity,
-- retrieve a cursor that describes the definition of that entity.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#gafcfbec461e561bf13f1e8540bbbd655b>
clang_getCursorDefinition :: CXCursor -> IO CXCursor
clang_getCursorDefinition cursor =
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_getCursorDefinition cursor'

-- |  Retrieve the canonical cursor corresponding to the given cursor.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#gac802826668be9fd40a017523cc7d24fe>
clang_getCanonicalCursor :: CXCursor -> IO CXCursor
clang_getCanonicalCursor cursor =
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_getCanonicalCursor cursor'

-- | Given a cursor that represents a declaration, return the associated comment
-- text, including comment markers.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#ga32905a8b1858e67cf5d28b7ad7150779>
clang_Cursor_getRawCommentText :: CXCursor -> IO Text
clang_Cursor_getRawCommentText cursor =
    onHaskellHeap cursor $ \cursor' ->
      preallocate_$ wrap_Cursor_getRawCommentText cursor'

-- | Given a cursor that represents a documentable entity (e.g., declaration),
-- return the associated brief comment.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#ga6b5282b915d457d728434c0651ea0b8b>
clang_Cursor_getBriefCommentText :: CXCursor -> IO Text
clang_Cursor_getBriefCommentText cursor =
    onHaskellHeap cursor $ \cursor' ->
      preallocate_$ wrap_Cursor_getBriefCommentText cursor'

-- | Retrieve a range for a piece that forms the cursors spelling name.
--
-- Most of the times there is only one range for the complete spelling but for
-- Objective-C methods and Objective-C message expressions, there are multiple
-- pieces for each selector identifier.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#ga251b31de80fd14681edf46f43b0bd03b>
clang_Cursor_getSpellingNameRange ::
     CXCursor
  -> CUInt
  -- ^ @pieceIndex@
  --
  -- The index of the spelling name piece. If this is greater than the actual
  -- number of pieces, it will return a NULL (invalid) range.
  -> CUInt
  -- ^ @options@
  --
  -- Reserved.
  -> IO CXSourceRange
clang_Cursor_getSpellingNameRange cursor pieceIndex options =
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_Cursor_getSpellingNameRange cursor' pieceIndex options

-- | Determine whether the declaration pointed to by this cursor is also a
-- definition of that entity.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#ga6ad05634a73e693217088eaa693f0010>
clang_isCursorDefinition :: CXCursor -> IO Bool
clang_isCursorDefinition cursor =
    onHaskellHeap cursor $ \cursor' ->
      cToBool <$> wrap_isCursorDefinition cursor'

{-------------------------------------------------------------------------------
  Type information for CXCursors

  <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html>
-------------------------------------------------------------------------------}

-- | The type of an element in the abstract syntax tree.
--
-- <https://clang.llvm.org/doxygen/structCXType.html>
newtype CXType = CXType (OnHaskellHeap CXType_)
  deriving newtype (LivesOnHaskellHeap, Preallocate, Eq, Ord, Show)

foreign import capi unsafe "clang_wrappers.h wrap_cxtKind"
  wrap_cxtKind :: R CXType_ -> IO (SimpleEnum CXTypeKind)

foreign import capi unsafe "clang_wrappers.h wrap_getCursorType"
  wrap_getCursorType :: R CXCursor_ -> W CXType_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getTypeKindSpelling"
  wrap_getTypeKindSpelling :: SimpleEnum CXTypeKind -> W CXString_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getTypeSpelling"
  wrap_getTypeSpelling :: R CXType_ -> W CXString_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getTypedefDeclUnderlyingType"
  wrap_getTypedefDeclUnderlyingType :: R CXCursor_ -> W CXType_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getPointeeType"
  wrap_getPointeeType :: R CXType_ -> W CXType_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_Type_getSizeOf"
  wrap_Type_getSizeOf :: R CXType_ -> IO CLLong

foreign import capi unsafe "clang_wrappers.h wrap_Type_getAlignOf"
  wrap_getAlignOf :: R CXType_ -> IO CLLong

foreign import capi unsafe "clang_wrappers.h wrap_Type_isTransparentTagTypedef"
  wrap_Type_isTransparentTagTypedef :: R CXType_ -> IO CUInt

foreign import capi unsafe "clang_wrappers.h wrap_Cursor_getOffsetOfField"
  wrap_Cursor_getOffsetOfField :: R CXCursor_ -> IO CLLong

foreign import capi unsafe "clang_wrappers.h wrap_Cursor_isAnonymous"
  wrap_Cursor_isAnonymous :: R CXCursor_ -> IO CUInt

foreign import capi unsafe "clang_wrappers.h wrap_getEnumConstantDeclValue"
  wrap_getEnumConstantDeclValue :: R CXCursor_ -> IO CLLong

foreign import capi unsafe "clang_wrappers.h"
  wrap_getCanonicalType :: R CXType_ -> W CXType_ -> IO ()

foreign import capi unsafe "clang_wrappers.h"
  wrap_getTypedefName :: R CXType_ -> W CXString_ -> IO ()

foreign import capi unsafe "clang_wrappers.h"
  wrap_getUnqualifiedType :: R CXType_ -> W CXType_ -> IO ()

foreign import capi unsafe "clang_wrappers.h"
  wrap_getTypeDeclaration :: R CXType_ -> W CXCursor_ -> IO ()

foreign import capi unsafe "clang_wrappers.h"
  wrap_Type_getNamedType :: R CXType_ -> W CXType_ -> IO ()

foreign import capi unsafe "clang_wrappers.h"
  wrap_Type_getModifiedType :: R CXType_ -> W CXType_ -> IO ()

foreign import capi unsafe "clang_wrappers.h"
  wrap_Type_getValueType :: R CXType_ -> W CXType_ -> IO ()

-- | Extract the @kind@ field from a @CXType@ struct
--
-- <https://clang.llvm.org/doxygen/structCXType.html#ab27a7510dc88b0ec80cff04ec89901aa>
cxtKind :: CXType -> SimpleEnum CXTypeKind
cxtKind typ = unsafePerformIO $
    onHaskellHeap typ $ \typ' ->
      wrap_cxtKind typ'

-- | Retrieve the type of a CXCursor (if any).
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gaae5702661bb1f2f93038051737de20f4>
clang_getCursorType :: CXCursor -> IO CXType
clang_getCursorType cursor =
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_getCursorType cursor'

-- | Retrieve the spelling of a given CXTypeKind.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga6bd7b366d998fc67f4178236398d0666>
clang_getTypeKindSpelling :: SimpleEnum CXTypeKind -> IO Text
clang_getTypeKindSpelling kind =
    preallocate_$ wrap_getTypeKindSpelling kind

-- | Pretty-print the underlying type using the rules of the language of the
-- translation unit from which it came.
--
-- Throws 'CallFailed' if the type is invalid.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gac9d37f61bede521d4f42a6553bcbc09f>
clang_getTypeSpelling :: HasCallStack => CXType -> IO Text
clang_getTypeSpelling typ = ensure (not . Text.null) $
     onHaskellHeap typ $ \typ' ->
       preallocate_$ wrap_getTypeSpelling typ'

-- | Retrieve the underlying type of a typedef declaration.
--
-- Throws 'CallFailed' if the cursor does not reference a typedef declaration.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga8de899fc18dc859b6fe3b97309f4fd52>
clang_getTypedefDeclUnderlyingType :: CXCursor -> IO CXType
clang_getTypedefDeclUnderlyingType cursor = ensureValidType $
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_getTypedefDeclUnderlyingType cursor'

-- | For pointer types, returns the type of the pointee.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gaafa3eb34932d8da1358d50ed949ff3ee>
clang_getPointeeType :: CXType -> IO CXType
clang_getPointeeType typ =
    onHaskellHeap typ $ \typ' ->
      preallocate_ $ wrap_getPointeeType typ'

-- | Return the size of a type in bytes as per @C++[expr.sizeof]@ standard.
--
-- Throws 'CallFailed' with 'CXTypeLayoutError' on error.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga027abe334546e80931905f31399d0a8b>
clang_Type_getSizeOf :: HasCallStack => CXType -> IO CLLong
clang_Type_getSizeOf typ =
    onHaskellHeap typ $ \typ' -> ensureNotInRange @CXTypeLayoutError $
      wrap_Type_getSizeOf typ'

-- | Return the alignment of a type in bytes as per C++[expr.alignof] standard.
--
-- Throws 'CallFailed' with 'CXTypeLayoutError' on error.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gaee56de66c69ab5605fe47e7c52497e31>
clang_Type_getAlignOf :: HasCallStack => CXType -> IO CLLong
clang_Type_getAlignOf typ =
    onHaskellHeap typ $ \typ' -> ensureNotInRange @CXTypeLayoutError $
      wrap_getAlignOf typ'

-- | Determine if a typedef is 'transparent' tag.
--
-- A typedef is considered 'transparent' if it shares a name and spelling
-- location with its underlying tag type, as is the case with the @NS_ENUM@
-- macro.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga9ac4ecb0e84f25b9f05d54c67353eba0>
clang_Type_isTransparentTagTypedef :: CXType -> IO Bool
clang_Type_isTransparentTagTypedef typ =
    onHaskellHeap typ $ \typ' ->
      cToBool <$> wrap_Type_isTransparentTagTypedef typ'

-- | Return the offset of the field represented by the Cursor.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gaa7e0f0ec320c645e971168ac39aa0cab>
clang_Cursor_getOffsetOfField :: CXCursor -> IO CLLong
clang_Cursor_getOffsetOfField cursor =
    onHaskellHeap cursor $ \cursor' ->
      wrap_Cursor_getOffsetOfField cursor'

-- | Determine whether the given cursor represents an anonymous tag or
-- namespace.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga6e0d2674d126fd43816ce3a80b592373>
clang_Cursor_isAnonymous :: CXCursor -> IO Bool
clang_Cursor_isAnonymous cursor =
    onHaskellHeap cursor $ \cursor' ->
      cToBool <$> wrap_Cursor_isAnonymous cursor'

-- | Retrieve the integer value of an enum constant declaration as a signed long
-- long.
--
-- Throws 'CallFailed' if the cursor does not reference an enum constant
-- declaration.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga6b8585818420e7512feb4c9d209b4f4d>
clang_getEnumConstantDeclValue :: HasCallStack => CXCursor -> IO CLLong
clang_getEnumConstantDeclValue cursor = do
    -- The @libclang@ docs state:
    --
    -- > If the cursor does not reference an enum constant declaration,
    -- > LLONG_MIN is returned. Since this is also potentially a valid constant
    -- > value, the kind of the cursor must be verified before calling this
    -- > function.
    cursorKind <- clang_getCursorKind cursor
    unless (cursorKind == simpleEnum CXCursor_EnumConstantDecl) $
      callFailed cursorKind

    onHaskellHeap cursor $ \cursor' ->
      wrap_getEnumConstantDeclValue cursor'

-- | Return the canonical type for a CXType.
--
-- Clang's type system explicitly models typedefs and all the ways a specific
-- type can be represented. The canonical type is the underlying type with all
-- the "sugar" removed. For example, if 'T' is a typedef for 'int', the
-- canonical type for 'T' would be 'int'.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gaa9815d77adc6823c58be0a0e32010f8c>
clang_getCanonicalType :: CXType -> IO CXType
clang_getCanonicalType typ =
    onHaskellHeap typ $ \typ' ->
      preallocate_ $ wrap_getCanonicalType typ'

-- | Returns the typedef name of the given type.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga7b8e66707c7f27550acfc2daeec527ed>
clang_getTypedefName :: CXType -> IO Text
clang_getTypedefName arg =
    onHaskellHeap arg $ \arg' ->
      preallocate_ $ wrap_getTypedefName arg'

-- | Retrieve the unqualified variant of the given type, removing as little sugar as possible.
--
-- For example, given the following series of typedefs:
--
-- > typedef int Integer;
-- > typedef const Integer CInteger;
-- > typedef CInteger DifferenceType;
--
-- Executing 'clang_getUnqualifiedType' on a CXType that represents
-- @DifferenceType@, will desugar to a type representing @Integer@, that has no
-- qualifiers.
--
-- And, executing 'clang_getUnqualifiedType' on the type of the first argument
-- of the following function declaration:
--
-- > void foo(const int);
--
-- Will return a type representing @int@, removing the @const@ qualifier.
--
-- Sugar over array types is not desugared.
--
-- A type can be checked for qualifiers with 'clang_isConstQualifiedType',
-- 'clang_isVolatileQualifiedType' and 'clang_isRestrictQualifiedType'.
--
-- A type that resulted from a call to 'clang_getUnqualifiedType' will return
-- false for all of the above calls.
--
-- Throws 'CallFailed' if the argument is an invalid type.
--
-- /NOTE/: Requires @llvm-16@ or higher; throws 'ClangVersionError' otherwise.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga8adac28955bf2f3a5ab1fd316a498334>
clang_getUnqualifiedType :: CXType -> IO CXType
clang_getUnqualifiedType typ = do
    -- clang_getUnqualifiedType was added in Clang 16
    requireClangVersion Clang16
    -- clang_getUnqualifiedType segfaults when CT is invalid
    case fromSimpleEnum (cxtKind typ) of
      e@Left{}                 -> callFailed e
      e@(Right CXType_Invalid) -> callFailed e
      Right{}                  -> pure ()
    onHaskellHeap typ $ \typ' ->
      preallocate_ $ wrap_getUnqualifiedType typ'

-- | Return the cursor for the declaration of the given type.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga0aad74ea93a2f5dea58fd6fc0db8aad4>
clang_getTypeDeclaration :: CXType -> IO CXCursor
clang_getTypeDeclaration typ =
    onHaskellHeap typ $ \typ' ->
      preallocate_ $ wrap_getTypeDeclaration typ'

-- | Retrieve the type named by the qualified-id.
--
-- Throws 'CallFailed' if a non-elaborated type is passed in.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gac6d90c2acdae77f75d8e8288658da463>
clang_Type_getNamedType :: HasCallStack => CXType -> IO CXType
clang_Type_getNamedType typ = ensureValidType $
    onHaskellHeap typ $ \typ' ->
      preallocate_ $ wrap_Type_getNamedType typ'

-- | Return the type that was modified by this attributed type.
--
-- Throws 'CallFailed' if the type is not an attributed type.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga6fc6ec9bfd9baada2d3fd6022d774675>
clang_Type_getModifiedType :: HasCallStack => CXType -> IO CXType
clang_Type_getModifiedType typ = ensureValidType $
    onHaskellHeap typ $ \typ' ->
      preallocate_ $ wrap_Type_getModifiedType typ'

-- | Gets the type contained by this atomic type.
--
-- Throws 'CallFailed' if a non-atomic type is passed in.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gae42d9886e0e221df03c4a518d9afb622>
clang_Type_getValueType :: HasCallStack => CXType -> IO CXType
clang_Type_getValueType typ = ensureValidType $
    onHaskellHeap typ $ \typ' ->
      preallocate_ $ wrap_Type_getValueType typ'

{-------------------------------------------------------------------------------
  Mapping between cursors and source code

  <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__SOURCE.html>
-------------------------------------------------------------------------------}

-- | Identifies a half-open character range in the source code.
--
-- Use 'clang_getRangeStart' and 'clang_getRangeEnd' to retrieve the starting
-- and end locations from a source range, respectively.
--
-- <https://clang.llvm.org/doxygen/structCXSourceRange.html>
newtype CXSourceRange = CXSourceRange (OnHaskellHeap CXSourceRange_)
  deriving newtype (LivesOnHaskellHeap, Preallocate)

foreign import capi unsafe "clang_wrappers.h wrap_getCursorLocation"
  wrap_getCursorLocation :: R CXCursor_ -> W CXSourceLocation_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getCursorExtent"
  wrap_getCursorExtent :: R CXCursor_ -> W CXSourceRange_ -> IO ()

-- | Retrieve the physical location of the source constructor referenced by the
-- given cursor.
--
-- The location of a declaration is typically the location of the name of that
-- declaration, where the name of that declaration would occur if it is unnamed,
-- or some keyword that introduces that particular declaration. The location of
-- a reference is where that reference occurs within the source code.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__SOURCE.html#gada3d3cbd3a3e83ff64f992617318dfb1>
clang_getCursorLocation :: CXCursor -> IO CXSourceLocation
clang_getCursorLocation cursor =
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_getCursorLocation cursor'

-- | Retrieve the physical extent of the source construct referenced by the
-- given cursor.
--
-- The extent of a cursor starts with the file/line/column pointing at the first
-- character within the source construct that the cursor refers to and ends with
-- the last character within that source construct. For a declaration, the
-- extent covers the declaration itself. For a reference, the extent covers the
-- location of the reference (e.g., where the referenced entity was actually
-- used).
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__SOURCE.html#ga79f6544534ab73c78a8494c4c0bc2840>
clang_getCursorExtent :: CXCursor -> IO CXSourceRange
clang_getCursorExtent cursor =
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_getCursorExtent cursor'

{-------------------------------------------------------------------------------
  Token extraction and manipulation

  <https://clang.llvm.org/doxygen/group__CINDEX__LEX.html>
-------------------------------------------------------------------------------}

newtype CXToken = CXToken (Ptr ())
  deriving stock (Show)

foreign import capi unsafe "clang_wrappers.h wrap_getToken"
  wrap_getToken :: CXTranslationUnit -> R CXSourceLocation_ -> IO CXToken

foreign import capi unsafe "clang_wrappers.h wrap_getTokenKind"
  clang_getTokenKind :: CXToken -> IO (SimpleEnum CXTokenKind)

foreign import capi unsafe "clang_wrappers.h wrap_getTokenSpelling"
  wrap_getTokenSpelling :: CXTranslationUnit -> CXToken -> W CXString_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getTokenLocation"
  wrap_getTokenLocation ::
       CXTranslationUnit
    -> CXToken
    -> W CXSourceLocation_
    -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getTokenExtent"
  wrap_getTokenExtent ::
       CXTranslationUnit
    -> CXToken
    -> W CXSourceRange_
    -> IO ()

-- | Get the raw lexical token starting with the given location.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LEX.html#ga3b41b2c8a34e605a14608927ae544c03>
clang_getToken :: CXTranslationUnit -> CXSourceLocation -> IO (Maybe CXToken)
clang_getToken unit loc = checkNotNull $
    onHaskellHeap loc $ \loc' ->
      wrap_getToken unit loc'

-- | Determine the spelling of the given token.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LEX.html#ga1033a25c9d2c59bcbdb23020de0bba2c>
clang_getTokenSpelling :: CXTranslationUnit -> CXToken -> IO Text
clang_getTokenSpelling unit token =
    preallocate_ $ wrap_getTokenSpelling unit token

-- | Retrieve the source location of the given token.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LEX.html#ga76a721514acb4cc523e10a6913d88021>
clang_getTokenLocation :: CXTranslationUnit -> CXToken -> IO CXSourceLocation
clang_getTokenLocation unit token =
    preallocate_ $ wrap_getTokenLocation unit token

-- | Retrieve a source range that covers the given token.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LEX.html#ga5acbc0a2a3c01aa44e1c5c5ccc4e328b>
clang_getTokenExtent :: CXTranslationUnit -> CXToken -> IO CXSourceRange
clang_getTokenExtent unit token =
    preallocate_ $ wrap_getTokenExtent unit token

newtype CXTokenArray = CXTokenArray (Ptr ())
  deriving newtype (Storable)

foreign import capi unsafe "clang_wrappers.h wrap_tokenize"
  wrap_tokenize ::
       CXTranslationUnit
       -- ^ the translation unit whose text is being tokenized.
    -> R CXSourceRange_
       -- ^ the source range in which text should be tokenized. All of the
       -- tokens produced by tokenization will fall within this source range
    -> Ptr CXTokenArray
       -- ^ this pointer will be set to point to the array of tokens that occur
       -- within the given source range. The returned pointer must be freed with
       -- clang_disposeTokens() before the translation unit is destroyed.
    -> Ptr CUInt
       -- ^ will be set to the number of tokens in the *Tokens array.
    -> IO ()

-- | Free the given set of tokens.
foreign import capi unsafe "clang-c/Index.h clang_disposeTokens"
  clang_disposeTokens :: CXTranslationUnit -> CXTokenArray -> CUInt -> IO ()

-- | Tokenize the source code described by the given range into raw lexical
-- tokens.
--
-- Returns the array of tokens and the number of tokens in the array. The array
-- must be disposed using 'clang_disponseTokens' before the translation unit is
-- destroyed.
clang_tokenize ::
     CXTranslationUnit
  -> CXSourceRange
  -> IO (CXTokenArray, CUInt)
clang_tokenize unit range =
    onHaskellHeap range $ \range' ->
      alloca $ \array ->
      alloca $ \numTokens -> do
        wrap_tokenize unit range' array numTokens
        (,) <$> peek array <*> peek numTokens

-- | Index token array
--
-- We do not verify bounds (nor that the array has not already been disposed).
index_CXTokenArray :: CXTokenArray -> CUInt -> CXToken
index_CXTokenArray (CXTokenArray array) i = CXToken $
    array `plusPtr` (fromIntegral i * knownSize @CXToken_)

newtype CXCursorArray = CXCursorArray (ArrOnHaskellHeap CXCursor_)

foreign import capi unsafe "clang-c/Index.h clang_annotateTokens"
  nowrapper_annotateTokens ::
       CXTranslationUnit
       -- ^ the translation unit that owns the given tokens.
    -> CXTokenArray
       -- ^ the set of tokens to annotate.
    -> CUInt
       -- ^ the number of tokens in Tokens.
    -> W CXCursor_
       -- ^ an array of NumTokens cursors, whose contents will be replaced with
       -- the cursors corresponding to each token.
    -> IO ()

clang_annotateTokens ::
     CXTranslationUnit
  -> CXTokenArray  -- ^ Tokens to annotate
  -> CUInt         -- ^ Number of tokens in the array
  -> IO CXCursorArray
clang_annotateTokens unit tokens numTokens = fmap CXCursorArray $
    preallocateArray (fromIntegral numTokens) $ \arr ->
      nowrapper_annotateTokens unit tokens numTokens arr

index_CXCursorArray :: CXCursorArray -> CUInt -> IO CXCursor
index_CXCursorArray (CXCursorArray arr) i =
    CXCursor <$> indexArrOnHaskellHeap arr (fromIntegral i)

{-------------------------------------------------------------------------------
  Physical source locations

  <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html>
-------------------------------------------------------------------------------}

-- | Identifies a specific source location within a translation unit.
--
-- Use 'clang_getExpansionLocation' or 'clang_getSpellingLocation' to map a
-- source location to a particular file, line, and column.
--
-- <https://clang.llvm.org/doxygen/structCXSourceLocation.html>
newtype CXSourceLocation = CXSourceLocation (OnHaskellHeap CXSourceLocation_)
  deriving newtype (LivesOnHaskellHeap, Preallocate)

-- | A particular source file that is part of a translation unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__FILES.html#gacfcea9c1239c916597e2e5b3e109215a>
newtype CXFile = CXFile (Ptr ())
  deriving stock (Show)
  deriving newtype (Storable)

foreign import capi unsafe "clang_wrappers.h wrap_getRangeStart"
  wrap_getRangeStart :: R CXSourceRange_ -> W CXSourceLocation_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getRangeEnd"
  wrap_getRangeEnd :: R CXSourceRange_ -> W CXSourceLocation_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getExpansionLocation"
  wrap_getExpansionLocation ::
       R CXSourceLocation_
       -- ^ the location within a source file that will be decomposed into its
       -- parts.
    -> Ptr CXFile
       -- ^ [out] if non-NULL, will be set to the file to which the given source
       -- location points.
    -> Ptr CUInt
       -- ^ [out] if non-NULL, will be set to the line to which the given source
       -- location points.
    -> Ptr CUInt
       -- ^ [out] if non-NULL, will be set to the column to which the given
       -- source location points.
    -> Ptr CUInt
       -- ^ [out] if non-NULL, will be set to the offset into the buffer to
       -- which the given source location points.
    -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getPresumedLocation"
  wrap_getPresumedLocation ::
       R CXSourceLocation_
       -- ^ the location within a source file that will be decomposed into its
       -- parts.
    -> W CXString_
       -- ^ [out] if non-NULL, will be set to the filename of the source
       -- location.
       --
       -- Note that filenames returned will be for "virtual" files, which don't
       -- necessarily exist on the machine running clang - e.g. when parsing
       -- preprocessed output obtained from a different environment. If a
       -- non-NULL value is passed in, remember to dispose of the returned value
       -- using clang_disposeString() once you've finished with it. For an
       -- invalid source location, an empty string is returned.
    -> Ptr CUInt
       -- ^ [out] if non-NULL, will be set to the line number of the source
       -- location. For an invalid source location, zero is returned.
    -> Ptr CUInt
       -- ^ [out] if non-NULL, will be set to the column number of the source
       -- location. For an invalid source location, zero is returned.
    -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getSpellingLocation"
  wrap_getSpellingLocation ::
       R CXSourceLocation_
       -- ^ the location within a source file that will be decomposed into its
       -- parts.
    -> Ptr CXFile
       -- ^ [out] if non-NULL, will be set to the file to which the given source
       -- location points.
    -> Ptr CUInt
       -- ^ [out] if non-NULL, will be set to the line to which the given source
       -- location points.
    -> Ptr CUInt
       -- ^ [out] if non-NULL, will be set to the column to which the given
       -- source location points.
    -> Ptr CUInt
       -- ^ [out] if non-NULL, will be set to the offset into the buffer to
       -- which the given source location points.
    -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getFileLocation"
  wrap_getFileLocation ::
       R CXSourceLocation_
       -- ^ the location within a source file that will be decomposed into its
       -- parts.
    -> Ptr CXFile
       -- ^ [out] if non-NULL, will be set to the file to which the given source
       -- location points.
    -> Ptr CUInt
       -- ^ [out] if non-NULL, will be set to the line to which the given source
       -- location points.
    -> Ptr CUInt
       -- ^ [out] if non-NULL, will be set to the column to which the given
       -- source location points.
    -> Ptr CUInt
       -- ^ [out] if non-NULL, will be set to the offset into the buffer to
       -- which the given source location points.
    -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getLocation"
  wrap_getLocation ::
       CXTranslationUnit
    -> CXFile
    -> CUInt
    -> CUInt
    -> W CXSourceLocation_
    -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getRange"
  wrap_getRange ::
       R CXSourceLocation_
    -> R CXSourceLocation_
    -> W CXSourceRange_
    -> IO ()

foreign import capi unsafe "clang-c/Index.h clang_getFile"
  nowrapper_getFile :: CXTranslationUnit -> CString -> IO CXFile

foreign import capi "clang_wrappers.h wrap_Location_isFromMainFile"
  wrap_Location_isFromMainFile :: R CXSourceLocation_ -> IO CInt

-- | Retrieve a source location representing the first character within a source
-- range.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#gac2cc034e3965739c41662f6ada7ff248>
clang_getRangeStart :: CXSourceRange -> IO CXSourceLocation
clang_getRangeStart range =
    onHaskellHeap range $ \range' ->
      preallocate_ $ wrap_getRangeStart range'

-- | Retrieve a source location representing the last character within a source
-- range.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#gacdb7d3c2b77a06bcc2e83bde3e14c3c0>
clang_getRangeEnd :: CXSourceRange -> IO CXSourceLocation
clang_getRangeEnd range =
    onHaskellHeap range $ \range' ->
      preallocate_ $ wrap_getRangeEnd range'

-- | Retrieve the file, line, column, and offset represented by the given source
-- location.
--
-- If the location refers into a macro expansion, retrieves the location of the
-- macro expansion.
--
-- NOTE: this replaces @clang_getInstantiationLocation@ (now legacy).
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#gadee4bea0fa34550663e869f48550eb1f>
clang_getExpansionLocation ::
     CXSourceLocation
  -> IO (CXFile, CUInt, CUInt, CUInt)
clang_getExpansionLocation location =
    onHaskellHeap location $ \location' ->
      alloca $ \file ->
      alloca $ \line ->
      alloca $ \column ->
      alloca $ \offset -> do
        wrap_getExpansionLocation location' file line column offset
        (,,,) <$> peek file <*> peek line <*> peek column <*> peek offset

-- | Retrieve the file, line and column represented by the given source
-- location, as specified in a @#line@ directive.
--
-- Note that filenames returned will be for "virtual" files, which don't
-- necessarily exist on the machine running clang - e.g. when parsing
-- preprocessed output obtained from a different environment.
--
-- Example: given the following source code in a file somefile.c
--
-- > #123 "dummy.c" 1
-- >
-- > static int func(void)
-- > {
-- >     return 0;
-- > }
--
-- the location information returned by this function would be
--
-- > File: dummy.c Line: 124 Column: 12
--
-- whereas 'clang_getExpansionLocation' would have returned
--
-- > File: somefile.c Line: 3 Column: 12
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#ga03508d9c944feeb3877515a1b08d36f9>
clang_getPresumedLocation :: CXSourceLocation -> IO (Text, CUInt, CUInt)
clang_getPresumedLocation location =
    onHaskellHeap location $ \location' ->
      alloca $ \line ->
      alloca $ \column -> do
        filename' <- preallocate_ $ \filename ->
          wrap_getPresumedLocation location' filename line column
        (filename',,) <$> peek line <*> peek column

-- | Retrieve the file, line, column, and offset represented by the given source
-- location.
--
-- If the location refers into a macro instantiation, return where the location
-- was originally spelled in the source file.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#ga01f1a342f7807ea742aedd2c61c46fa0>
clang_getSpellingLocation ::
     CXSourceLocation
  -> IO (CXFile, CUInt, CUInt, CUInt)
clang_getSpellingLocation location =
    onHaskellHeap location $ \location' ->
      alloca $ \file ->
      alloca $ \line ->
      alloca $ \column ->
      alloca $ \offset -> do
        wrap_getSpellingLocation location' file line column offset
        (,,,) <$> peek file <*> peek line <*> peek column <*> peek offset

-- | Retrieve the file, line, column, and offset represented by the given source
-- location.
--
-- If the location refers into a macro expansion, return where the macro was
-- expanded or where the macro argument was written, if the location points at a
-- macro argument.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#gae0ee9ff0ea04f2446832fc12a7fd2ac8>
clang_getFileLocation ::
     CXSourceLocation
  -> IO (CXFile, CUInt, CUInt, CUInt)
clang_getFileLocation location =
    onHaskellHeap location $ \location' ->
      alloca $ \file ->
      alloca $ \line ->
      alloca $ \column ->
      alloca $ \offset -> do
        wrap_getFileLocation location' file line column offset
        (,,,) <$> peek file <*> peek line <*> peek column <*> peek offset

-- | Retrieves the source location associated with a given file/line/column in a
-- particular translation unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#ga86d822034407d60d9e1f36e07cbc0f67>
clang_getLocation ::
     CXTranslationUnit
  -> CXFile
  -> CUInt   -- ^ Line
  -> CUInt   -- ^ Column
  -> IO CXSourceLocation
clang_getLocation unit file line col =
    preallocate_ $ wrap_getLocation unit file line col

-- | Retrieve a source range given the beginning and ending source locations.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#ga4e2b6d439f72fdee12c2e4dcf4ff1e2f>
clang_getRange :: CXSourceLocation -> CXSourceLocation -> IO CXSourceRange
clang_getRange begin end =
    onHaskellHeap begin $ \begin' ->
    onHaskellHeap end   $ \end' ->
      preallocate_ $ wrap_getRange begin' end'

-- | Retrieve a file handle within the given translation unit.
--
-- Returns the file handle for the named file in the translation unit.
-- Throws 'CallFailed' if the file was not a part of this translation unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#gaa0554e2ea48ecd217a29314d3cbd2085>
clang_getFile :: CXTranslationUnit -> Text -> IO CXFile
clang_getFile unit file = ensureNotNull $
    withCString (Text.unpack file) $ \file' ->
      nowrapper_getFile unit file'

-- | Check if the given source location is in the main file of the corresponding
-- translation unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#gacb4ca7b858d66f0205797ae84cc4e8f2>
clang_Location_isFromMainFile :: CXSourceLocation -> IO Bool
clang_Location_isFromMainFile location =
    onHaskellHeap location $ \location' ->
      cToBool <$> wrap_Location_isFromMainFile location'

{-------------------------------------------------------------------------------
  File manipulation routines

  <https://clang.llvm.org/doxygen/group__CINDEX__FILES.html>
-------------------------------------------------------------------------------}

foreign import capi "clang_wrappers.h wrap_getFileName"
  wrap_getFileName :: CXFile -> W CXString_ -> IO ()

-- | Retrieve the complete file and path name of the given file.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__FILES.html#ga626ff6335ab1e0a2b8c8823301225690>
clang_getFileName :: CXFile -> IO Text
clang_getFileName file = preallocate_$ wrap_getFileName file

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

-- | Debugging breakpoint hook
--
-- Every call to @clang_breakpoint@ prints
--
-- > clang_breakpoint: <count>
--
-- to @stderr@, for an ever increasing @<count>@ (starting at 1). This is useful
-- for debugging; for example, if you want a breakpoint on the 13th invocation:
--
-- > break clang_breakpoint
-- > ignore 1 12
foreign import capi "clang_wrappers.h clang_breakpoint"
  clang_breakpoint :: IO ()

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ensureValidType :: HasCallStack => IO CXType -> IO CXType
ensureValidType = ensure (aux . fromSimpleEnum . cxtKind)
  where
    aux :: Either CInt CXTypeKind -> Bool
    aux (Left _)               = False
    aux (Right CXType_Invalid) = False
    aux _otherwise             = True

