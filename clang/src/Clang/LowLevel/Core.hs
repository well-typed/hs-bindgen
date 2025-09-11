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
--   "Clang.Internal.ByValue".
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
-- /Note on pointers/: in the public API, all @libclang@ types are opaque.
-- Internally, they are all newtypes around either 'Ptr' or 'OnHaskellHeap',
-- depending on whether or not we own the value.
module Clang.LowLevel.Core (
    -- * Top-level
    CXIndex
  , DisplayDiagnostics(..)
  , clang_createIndex
  , clang_disposeIndex
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
  , CXUnsavedFile(..)
  , CXTranslationUnit_Flags(..)
  , CXTargetInfo(..)
  , CXErrorCode(..)
  , clang_parseTranslationUnit
  , clang_parseTranslationUnit2
  , clang_disposeTranslationUnit
  , clang_getTranslationUnitTargetInfo
  , clang_TargetInfo_dispose
  , clang_TargetInfo_getTriple
    -- * Cursor manipulations
  , CXCursor(..)
  , CXCursorKind(..)
  , CXTLSKind(..)
  , CXLinkageKind(..)
  , CXVisibilityKind(..)
  , clang_getTranslationUnitCursor
  , clang_equalCursors
  , clang_getCursorSemanticParent
  , clang_getCursorLexicalParent
  , clang_getCursorTLSKind
  , clang_Cursor_getArgument
  , clang_getNullCursor
  , clang_getCursorKind
  , clang_getCursorKindSpelling
  , clang_Cursor_getTranslationUnit
  , clang_isDeclaration
  , clang_getCursorLinkage
  , clang_getCursorVisibility
  , clang_getIncludedFile
  , clang_Cursor_getVarDeclInitializer
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
  , clang_getCursorPrintingPolicy
  , clang_getCursorPrettyPrinted
  , clang_PrintingPolicy_dispose
    -- * Type information for CXCursors
  , CXTypeKind(..)
  , CXTypeLayoutError(..)
  , CXType(..)
  , CX_StorageClass(..)
  , cxtKind
  , clang_getCursorType
  , clang_getTypeKindSpelling
  , clang_getTypeSpelling
  , clang_getTypedefDeclUnderlyingType
  , clang_getEnumDeclIntegerType
  , clang_Cursor_isBitField
  , clang_getFieldDeclBitWidth
  , clang_getPointeeType
  , clang_getElementType
  , clang_getArrayElementType
  , clang_getArraySize
  , clang_Type_getSizeOf
  , clang_Type_getAlignOf
  , clang_Type_isTransparentTagTypedef
  , clang_Cursor_getOffsetOfField
  , clang_Cursor_getStorageClass
  , clang_Cursor_isAnonymous
  , clang_Cursor_isAnonymousRecordDecl
  , clang_getEnumConstantDeclValue
  , clang_getCanonicalType
  , clang_getTypedefName
  , clang_getUnqualifiedType
  , clang_isConstQualifiedType
  , clang_isVolatileQualifiedType
  , clang_isRestrictQualifiedType
  , clang_getTypeDeclaration
  , clang_isFunctionTypeVariadic
  , clang_getResultType
  , clang_getNumArgTypes
  , clang_getArgType
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
  , clang_Range_isNull
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
    -- * Rewrite API
  , clang_CXRewriter_create
  , clang_CXRewriter_insertTextBefore
  , clang_CXRewriter_writeMainFileToStdOut
  , clang_CXRewriter_dispose
    -- * Auxiliary
  , nullCursor
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as Text
import Foreign
import Foreign.C
import GHC.Stack
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)

import Clang.Args
import Clang.Enum.Bitfield
import Clang.Enum.Simple
import Clang.Internal.ByValue
import Clang.Internal.CXString ()
import Clang.Internal.FFI
import Clang.Internal.Results
import Clang.LowLevel.Core.Enums
import Clang.LowLevel.Core.Instances ()
import Clang.LowLevel.Core.Pointers
import Clang.LowLevel.Core.Structs
import Clang.LowLevel.FFI
import Clang.Paths
import Clang.Version

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
  deriving stock (Show, Eq)

foreign import capi unsafe "clang-c/Index.h clang_createIndex"
  nowrapper_clang_createIndex ::
       CInt -- ^ @excludeDeclarationsFromPCH@
    -> CInt -- ^ @displayDiagnostics@
    -> IO CXIndex

foreign import capi unsafe "clang-c/Index.h clang_disposeIndex"
  nowrapper_disposeIndex :: CXIndex -> IO ()

foreign import capi unsafe "clang-c/Index.h clang_getNumDiagnostics"
  nowrapper_getNumDiagnostics :: CXTranslationUnit -> IO CUInt

foreign import capi unsafe "clang-c/Index.h clang_getDiagnostic"
  nowrapper_getDiagnostic ::
       CXTranslationUnit -- ^ the translation unit to query.
    -> CUInt             -- ^ the zero-based diagnostic number to retrieve.
    -> IO CXDiagnostic

-- | Provides a shared context for creating translation units.
--
-- /NOTE/: We are not planning to support precompiled headers, so we omit the
-- first argument (@excludeDeclarationsFromPCH@).
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#ga51eb9b38c18743bf2d824c6230e61f93>
clang_createIndex ::
     MonadIO m
  => DisplayDiagnostics
  -> m CXIndex
clang_createIndex diagnostics = liftIO $
    nowrapper_clang_createIndex 0 diagnostics'
  where
    diagnostics' :: CInt
    diagnostics' =
        case diagnostics of
          DisplayDiagnostics     -> 0
          DontDisplayDiagnostics -> 0

-- | Destroy the given index.
--
-- The index must not be destroyed until all of the translation units created
-- within that index have been destroyed.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#ga166ab73b14be73cbdcae14d62dbab22a>
clang_disposeIndex :: MonadIO m => CXIndex -> m ()
clang_disposeIndex cIdx = liftIO $ nowrapper_disposeIndex cIdx

-- | Determine the number of diagnostics produced for the given translation unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#gae9f047b4bbbbb01161478d549b7aab25>
clang_getNumDiagnostics :: MonadIO m => CXTranslationUnit -> m CUInt
clang_getNumDiagnostics unit = liftIO $ nowrapper_getNumDiagnostics unit

-- | Retrieve a diagnostic associated with the given translation unit.
--
-- Returns the requested diagnostic. This diagnostic must be freed via a call to
-- 'clang_disposeDiagnostic'.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#ga3f54a79e820c2ac9388611e98029afe5>
clang_getDiagnostic ::
     MonadIO m
  => CXTranslationUnit
     -- ^ the translation unit to query.
  -> CUInt
     -- ^ the zero-based diagnostic number to retrieve.
  -> m CXDiagnostic
clang_getDiagnostic unit ix = liftIO $ nowrapper_getDiagnostic unit ix

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

foreign import capi unsafe "clang-c/Index.h clang_getNumDiagnosticsInSet"
  nowrapper_getNumDiagnosticsInSet :: CXDiagnosticSet -> IO CUInt

foreign import capi unsafe "clang-c/Index.h clang_getDiagnosticInSet"
  nowrapper_getDiagnosticInSet ::
       CXDiagnosticSet  -- ^ the CXDiagnosticSet to query.
    -> CUInt            -- ^ the zero-based diagnostic number to retrieve.
    -> IO CXDiagnostic

foreign import capi unsafe "clang-c/Index.h clang_disposeDiagnosticSet"
  nowrapper_disposeDiagnosticSet :: CXDiagnosticSet -> IO ()

foreign import capi unsafe "clang-c/Index.h clang_getChildDiagnostics"
  nowrapper_getChildDiagnostics :: CXDiagnostic -> IO CXDiagnosticSet

foreign import capi unsafe "clang-c/Index.h clang_disposeDiagnostic"
  nowrapper_disposeDiagnostic :: CXDiagnostic -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_formatDiagnostic"
  wrap_formatDiagnostic ::
       CXDiagnostic
    -> BitfieldEnum CXDiagnosticDisplayOptions
    -> W CXString_
    -> IO ()

foreign import capi unsafe "clang-c/Index.h clang_defaultDiagnosticDisplayOptions"
  nowrapper_defaultDiagnosticDisplayOptions ::
       IO (BitfieldEnum CXDiagnosticDisplayOptions)

foreign import capi unsafe "clang-c/Index.h clang_getDiagnosticSeverity"
  nowrapper_getDiagnosticSeverity ::
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

foreign import capi unsafe "clang-c/Index.h clang_getDiagnosticCategory"
  nowrapper_getDiagnosticCategory :: CXDiagnostic -> IO CUInt

foreign import capi unsafe "clang_wrappers.h wrap_getDiagnosticCategoryText"
  wrap_getDiagnosticCategoryText :: CXDiagnostic -> W CXString_ -> IO ()

foreign import capi unsafe "clang-c/Index.h clang_getDiagnosticNumRanges"
  nowrapper_getDiagnosticNumRanges :: CXDiagnostic -> IO CUInt

foreign import capi unsafe "clang_wrappers.h wrap_getDiagnosticRange"
  wrap_getDiagnosticRange :: CXDiagnostic -> CUInt -> W CXSourceRange_ -> IO ()

foreign import capi unsafe "clang-c/Index.h clang_getDiagnosticNumFixIts"
  nowrapper_getDiagnosticNumFixIts :: CXDiagnostic -> IO CUInt

foreign import capi unsafe "clang_wrappers.h wrap_getDiagnosticFixIt"
  wrap_getDiagnosticFixIt ::
       CXDiagnostic
    -> CUInt
    -> W CXSourceRange_
    -> W CXString_
    -> IO ()

-- | Determine the number of diagnostics in a 'CXDiagnosticSet'.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga44e87e54125e501de0d3bd29161fe26b>
clang_getNumDiagnosticsInSet :: MonadIO m => CXDiagnosticSet -> m CUInt
clang_getNumDiagnosticsInSet set = liftIO $
    nowrapper_getNumDiagnosticsInSet set

-- | Retrieve a diagnostic associated with the given 'CXDiagnosticSet'.
--
-- Returns the requested diagnostic. This diagnostic must be freed via a call to
-- 'clang_disposeDiagnostic'.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga997e07d587e02eea7d29874c33c94249>
clang_getDiagnosticInSet ::
     MonadIO m
  => CXDiagnosticSet  -- ^ the CXDiagnosticSet to query.
  -> CUInt            -- ^ the zero-based diagnostic number to retrieve.
  -> m CXDiagnostic
clang_getDiagnosticInSet set ix = liftIO $
    nowrapper_getDiagnosticInSet set ix

-- | Release a CXDiagnosticSet and all of its contained diagnostics.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga1a1126b07e4dc0b45b0617f3cc848d57>
clang_disposeDiagnosticSet :: MonadIO m => CXDiagnosticSet -> m ()
clang_disposeDiagnosticSet set = liftIO $
    nowrapper_disposeDiagnosticSet set

-- | Retrieve the child diagnostics of a CXDiagnostic.
--
-- This 'CXDiagnosticSet' does not need to be released by
-- 'clang_disposeDiagnosticSet'.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga1aa24f925b34bb988dc3ea06ec27dcda>
clang_getChildDiagnostics :: MonadIO m => CXDiagnostic -> m CXDiagnosticSet
clang_getChildDiagnostics diagnostic = liftIO $
    nowrapper_getChildDiagnostics diagnostic

-- | Destroy a diagnostic.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga07061e0ad7665b7c5ee7253cd1bf4a5c>
clang_disposeDiagnostic :: MonadIO m => CXDiagnostic -> m ()
clang_disposeDiagnostic diagnostic = liftIO $
    nowrapper_disposeDiagnostic diagnostic

-- | Format the given diagnostic in a manner that is suitable for display.
--
-- This routine will format the given diagnostic to a string, rendering the
-- diagnostic according to the various options given. The
-- 'clang_defaultDiagnosticDisplayOptions' function returns the set of options
-- that most closely mimics the behavior of the clang compiler.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga455234ab6de0ca12c9ea36f8874060e8>
clang_formatDiagnostic ::
     MonadIO m
  => CXDiagnostic
  -> BitfieldEnum CXDiagnosticDisplayOptions
  -> m Text
clang_formatDiagnostic diagnostic options = liftIO $
    preallocate_ $ wrap_formatDiagnostic diagnostic options

-- | Retrieve the set of display options most similar to the default behavior of
-- the clang compiler.
--
-- Returns a set of display options suitable for use with
-- 'clang_formatDiagnostic'.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga5fcf910792541399efd63c62042ce353>
clang_defaultDiagnosticDisplayOptions ::
     MonadIO m
  => m (BitfieldEnum CXDiagnosticDisplayOptions)
clang_defaultDiagnosticDisplayOptions = liftIO $
    nowrapper_defaultDiagnosticDisplayOptions

-- | Determine the severity of the given diagnostic.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#gaff14261578eb9a2b02084f0cc6b95f9a>
clang_getDiagnosticSeverity ::
     MonadIO m
  => CXDiagnostic
  -> m (SimpleEnum CXDiagnosticSeverity)
clang_getDiagnosticSeverity diagnostic = liftIO $
    nowrapper_getDiagnosticSeverity diagnostic

-- | Retrieve the source location of the given diagnostic.
--
-- This location is where Clang would print the caret (@^@) when displaying the
-- diagnostic on the command line.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#gabfcf70ac15bb3e5ae39ef2c5e07c7428>
clang_getDiagnosticLocation :: MonadIO m => CXDiagnostic -> m CXSourceLocation
clang_getDiagnosticLocation diagnostic = liftIO $
    preallocate_ $ wrap_getDiagnosticLocation diagnostic

-- | Retrieve the text of the given diagnostic.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga34a875e6d06ed4f8d2fc032f850ebbe1>
clang_getDiagnosticSpelling :: MonadIO m => CXDiagnostic -> m Text
clang_getDiagnosticSpelling diagnostic = liftIO $
    preallocate_ $ wrap_getDiagnosticSpelling diagnostic

-- | Retrieve the name of the command-line option that enabled this diagnostic.
--
-- Returns a string that contains the command-line option used to enable this
-- warning, such as @"-Wconversion"@ or @"-pedantic"@, as well as the option
-- that disables this diagnostic (if any).
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga69b094e2cca1cd6f452327dc9204a168>
clang_getDiagnosticOption :: MonadIO m => CXDiagnostic -> m (Text, Text)
clang_getDiagnosticOption diagnostic = liftIO $
    preallocatePair_ $ wrap_getDiagnosticOption diagnostic

-- | Retrieve the category number for this diagnostic.
--
-- Diagnostics can be categorized into groups along with other, related
-- diagnostics (e.g., diagnostics under the same warning flag). This routine
-- retrieves the category number for the given diagnostic.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga0ec085bd59b8b6c935eab0e53a1f348f>
clang_getDiagnosticCategory :: MonadIO m => CXDiagnostic -> m CUInt
clang_getDiagnosticCategory diagnostic = liftIO $
    nowrapper_getDiagnosticCategory diagnostic

-- | Retrieve the diagnostic category text for a given diagnostic.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga6950702b6122f1cd74e1a369605a9f54>>
clang_getDiagnosticCategoryText :: MonadIO m => CXDiagnostic -> m Text
clang_getDiagnosticCategoryText diagnostic = liftIO $
    preallocate_ $ wrap_getDiagnosticCategoryText diagnostic

-- | Determine the number of source ranges associated with the given diagnostic.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga7acbd761f1113ea657022e5708694924>
clang_getDiagnosticNumRanges :: MonadIO m => CXDiagnostic -> m CUInt
clang_getDiagnosticNumRanges diagnostic = liftIO $
    nowrapper_getDiagnosticNumRanges diagnostic

-- | Retrieve a source range associated with the diagnostic.
--
-- A diagnostic's source ranges highlight important elements in the source code.
-- On the command line, Clang displays source ranges by underlining them with
-- @~@ characters.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#gabd440f1577374289ffebe73d9f65b294>
clang_getDiagnosticRange ::
     MonadIO m
  => CXDiagnostic  -- ^ the diagnostic whose range is being extracted.
  -> CUInt         -- ^ the zero-based index specifying which range to extract
  -> m CXSourceRange
clang_getDiagnosticRange diagnostic range = liftIO $
    preallocate_ $ wrap_getDiagnosticRange diagnostic range

-- | Determine the number of fix-it hints associated with the given diagnostic.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#gafe38dfd661f6ba59df956dfeabece2a2>
clang_getDiagnosticNumFixIts :: MonadIO m => CXDiagnostic -> m CUInt
clang_getDiagnosticNumFixIts diagnostic = liftIO $
    nowrapper_getDiagnosticNumFixIts diagnostic

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
     MonadIO m
  => CXDiagnostic  -- ^ The diagnostic whose fix-its are being queried.
  -> CUInt         -- ^ The zero-based index of the fix-it.
  -> m (CXSourceRange, Text)
clang_getDiagnosticFixIt diagnostic fixit = liftIO $
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
  deriving newtype (Storable)

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
    -> Ptr CXUnsavedFile
    -> CUInt
    -> BitfieldEnum CXTranslationUnit_Flags
    -> IO CXTranslationUnit

foreign import ccall unsafe "clang-c/Index.h clang_parseTranslationUnit2"
  nowrapper_parseTranslationUnit2 ::
       CXIndex
    -> CString
    -> Ptr CString
    -> CInt
    -> Ptr CXUnsavedFile
    -> CUInt
    -> BitfieldEnum CXTranslationUnit_Flags
    -> Ptr CXTranslationUnit
    -> IO (SimpleEnum (Maybe CXErrorCode))

foreign import capi "clang-c/Index.h clang_disposeTranslationUnit"
  nowrapper_disposeTranslationUnit :: CXTranslationUnit -> IO ()

foreign import capi "clang_wrappers.h clang_getTranslationUnitTargetInfo"
  nowrapper_getTranslationUnitTargetInfo :: CXTranslationUnit -> IO CXTargetInfo

foreign import capi "clang_wrappers.h clang_TargetInfo_dispose"
  nowrapper_TargetInfo_dispose :: CXTargetInfo -> IO ()

foreign import capi "clang_wrappers.h wrap_TargetInfo_getTriple"
  wrap_TargetInfo_getTriple :: CXTargetInfo -> W CXString_ -> IO ()

-- | Same as 'clang_parseTranslationUnit2', but returns the 'CXTranslationUnit'
-- instead of an error code.
--
-- Throws 'CallFailed' in case of an error.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TRANSLATION__UNIT.html#ga2baf83f8c3299788234c8bce55e4472e>
clang_parseTranslationUnit ::
     (MonadIO m, HasCallStack)
  => CXIndex                               -- ^ @CIdx@
  -> Maybe SourcePath                      -- ^ @source_filename@
  -> ClangArgs                             -- ^ @command_line_args@
  -> [CXUnsavedFile]                       -- ^ @unsaved_files@
  -> BitfieldEnum CXTranslationUnit_Flags  -- ^ @options@
  -> m CXTranslationUnit
clang_parseTranslationUnit cIdx src args unsavedFiles options = liftIO $
    withOptCString (getSourcePath <$> src) $ \src' ->
      withCStrings (unClangArgs args) $ \args' numArgs ->
        withArrayOrNull unsavedFiles $ \unsavedFiles' numUnsavedFiles ->
          ensureNotNull $
            nowrapper_parseTranslationUnit
              cIdx
              src'
              args'
              numArgs
              unsavedFiles'
              (fromIntegral numUnsavedFiles)
              options

-- | Parse the given source file and the translation unit corresponding to that
-- file.
--
-- This routine is the main entry point for the Clang C API, providing the
-- ability to parse a source file into a translation unit that can then be
-- queried by other functions in the API. This routine accepts a set of
-- command-line arguments so that the compilation can be configured in the same
-- way that the compiler is configured on the command line.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TRANSLATION__UNIT.html#ga494de0e725c5ae40cbdea5fa6081027d>
clang_parseTranslationUnit2 ::
     MonadIO m
  => CXIndex
     -- ^ The index object with which the translation unit will be associated.
  -> Maybe SourcePath
     -- ^ The name of the source file to load, or 'Nothing' if the source file
     -- is included in @command_line_args@.
  -> ClangArgs
     -- ^ The command-line arguments that would be passed to the clang
     -- executable if it were being invoked out-of-process.
     --
     -- These command-line options will be parsed and will affect how the
     -- translation unit is parsed. Note that the following options are ignored:
     -- @'-c'@, @'-emit-ast'@, @'-fsyntax-only'@ (which is the default), and
     -- @'-o <output file>'@.
  -> [CXUnsavedFile]
     -- ^ The files that have not yet been saved to disk but may be required for
     -- parsing, including the contents of those files.
     --
     -- The contents and name of these files (as specified by CXUnsavedFile) are
     -- copied when necessary, so the client only needs to guarantee their
     -- validity until the call to this function returns.
  -> BitfieldEnum CXTranslationUnit_Flags
     -- ^ Options that affects how the translation unit is managed but not its
     -- compilation.
  -> m (Either (SimpleEnum CXErrorCode) CXTranslationUnit)
clang_parseTranslationUnit2 cIdx src args unsavedFiles options = liftIO $
    withOptCString (getSourcePath <$> src) $ \src' ->
      withCStrings (unClangArgs args) $ \args' numArgs ->
        withArrayOrNull unsavedFiles $ \unsavedFiles' numUnsavedFiles ->
          alloca $ \outPtr -> do
            mError <- nowrapper_parseTranslationUnit2
              cIdx
              src'
              args'
              numArgs
              unsavedFiles'
              (fromIntegral numUnsavedFiles)
              options
              outPtr
            case fromSimpleEnum mError of
              Right Nothing ->
                Right <$> peek outPtr
              Right (Just knownError) ->
                return $ Left (simpleEnum knownError)
              Left unknownError ->
                return $ Left (coerceSimpleEnum unknownError)

-- | Destroy the specified CXTranslationUnit object.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TRANSLATION__UNIT.html#gaee753cb0036ca4ab59e48e3dff5f530a>
clang_disposeTranslationUnit :: MonadIO m => CXTranslationUnit -> m ()
clang_disposeTranslationUnit unit = liftIO $
    nowrapper_disposeTranslationUnit unit

-- | Get target information for this translation unit.
--
-- The 'CXTargetInfo' object cannot outlive the 'CXTranslationUnit' object.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TRANSLATION__UNIT.html#ga1813b53c06775c354f4797a5ec051948>
clang_getTranslationUnitTargetInfo :: MonadIO m
   => CXTranslationUnit -> m CXTargetInfo
clang_getTranslationUnitTargetInfo unit = liftIO $
    nowrapper_getTranslationUnitTargetInfo unit

-- | Destroy the 'CXTargetInfo' object.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TRANSLATION__UNIT.html#gafb00d82420b0101c185b88338567ffd9>
clang_TargetInfo_dispose :: MonadIO m => CXTargetInfo -> m ()
clang_TargetInfo_dispose info = liftIO $ nowrapper_TargetInfo_dispose info

--
-- | Get the normalized target triple as a string.
--
-- Throws 'CallFailed' on error.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TRANSLATION__UNIT.html#ga7ae67e3c8baf6a9852900f6529dce2d0>
clang_TargetInfo_getTriple ::
     (MonadIO m, HasCallStack)
  => CXTargetInfo -> m Text
clang_TargetInfo_getTriple info = liftIO $ ensure (not . Text.null) $
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
  deriving newtype (LivesOnHaskellHeap, Preallocate, Show)

-- |
--
-- Note: we cannot easily implement cursor comparison directly in Haskell, as it's slightly complicated:
-- See https://github.com/llvm/llvm-project/blob/4872ecf1cc3cb9c4939a9e6210a9b9e9a9032e9f/clang/tools/libclang/CIndex.cpp#L6529
instance Eq CXCursor where
  x == y = unsafeDupablePerformIO $ clang_equalCursors x y

foreign import capi unsafe "clang_wrappers.h wrap_getTranslationUnitCursor"
  wrap_getTranslationUnitCursor :: CXTranslationUnit -> W CXCursor_ -> IO ()

foreign import capi unsafe "wrap_Cursor_getTranslationUnit"
  wrap_Cursor_getTranslationUnit :: R CXCursor_ -> IO CXTranslationUnit

foreign import capi unsafe "wrap_getCursorTLSKind"
  wrap_getCursorTLSKind :: R CXCursor_ -> IO (SimpleEnum CXTLSKind)

-- | Retrieve the cursor that represents the given translation unit.
--
-- The translation unit cursor can be used to start traversing the various
-- declarations within the given translation unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#gaec6e69127920785e74e4a517423f4391>
clang_getTranslationUnitCursor :: MonadIO m => CXTranslationUnit -> m CXCursor
clang_getTranslationUnitCursor unit = liftIO $
    preallocate_ $ wrap_getTranslationUnitCursor unit

-- | Determine whether two cursors are equivalent.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#ga98df58f09878710b983b6f3f60f0cba3>
clang_equalCursors :: MonadIO m => CXCursor -> CXCursor -> m Bool
clang_equalCursors a b = liftIO $
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
clang_getCursorSemanticParent :: MonadIO m => CXCursor -> m CXCursor
clang_getCursorSemanticParent cursor = liftIO $
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
clang_getCursorLexicalParent :: MonadIO m => CXCursor -> m CXCursor
clang_getCursorLexicalParent cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_getCursorLexicalParent cursor'

-- | Determine the "thread-local storage (TLS) kind" of the declaration referred to by a cursor.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#ga524e1bd046dfb581484ec50e8f22ae7a>
clang_getCursorTLSKind :: MonadIO m => CXCursor -> m (SimpleEnum CXTLSKind)
clang_getCursorTLSKind cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      wrap_getCursorTLSKind cursor'

clang_Cursor_getArgument :: MonadIO m => CXCursor -> Int -> m CXCursor
clang_Cursor_getArgument cursor i = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_Cursor_getArgument cursor' (fromIntegral i)

-- | Retrieve the NULL cursor, which represents no entity.
clang_getNullCursor :: MonadIO m => m CXCursor
clang_getNullCursor = liftIO $ preallocate_ wrap_getNullCursor

-- | Retrieve the kind of the given cursor.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#ga018aaf60362cb751e517d9f8620d490c>
clang_getCursorKind :: MonadIO m => CXCursor -> m (SimpleEnum CXCursorKind)
clang_getCursorKind cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      wrap_getCursorKind cursor'

-- | Get spelling of a cursor
--
-- NOTE: This is from the @libclang@ \"Debugging facilities\"
-- (https://clang.llvm.org/doxygen/group__CINDEX__DEBUG.html). This should be
-- used only for testing and debugging, and should not be relied upon.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DEBUG.html#ga7a4eecfc1b343568cb9ea447cbde08a8
clang_getCursorKindSpelling :: MonadIO m => SimpleEnum CXCursorKind -> m Text
clang_getCursorKindSpelling kind = liftIO $
    preallocate_ $ wrap_getCursorKindSpelling kind

-- | Returns the translation unit that a cursor originated from.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#ga529f1504710a41ce358d4e8c3161848d>
clang_Cursor_getTranslationUnit ::
     MonadIO m
  => CXCursor -> m (Maybe CXTranslationUnit)
clang_Cursor_getTranslationUnit cursor = liftIO $ checkNotNull $
    onHaskellHeap cursor $ \cursor' ->
      wrap_Cursor_getTranslationUnit cursor'

-- | Determine whether the given cursor kind represents a declaration.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#ga660aa4846fce0a54e20073ab6a5465a0>
clang_isDeclaration :: MonadIO m => SimpleEnum CXCursorKind -> m Bool
clang_isDeclaration kind = liftIO $ cToBool <$> nowrapper_isDeclaration kind

-- | Determine the linkage of the entity referred to by a given cursor.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#ga359dae25aa1a71176a5e33f3c7ee1740>
clang_getCursorLinkage :: MonadIO m => CXCursor -> m (SimpleEnum CXLinkageKind)
clang_getCursorLinkage cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      wrap_getCursorLinkage cursor'

-- | Describe the visibility of the entity referred to by a cursor.
--
-- This returns the default visibility if not explicitly specified by a
-- visibility attribute. The default visibility may be changed by commandline
-- arguments.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#ga935b442bd6bde168cf354b7629b471d8>
clang_getCursorVisibility :: MonadIO m => CXCursor -> m (SimpleEnum CXVisibilityKind)
clang_getCursorVisibility cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      wrap_getCursorVisibility cursor'

-- | Retrieve the file that is included by the given inclusion directive cursor.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#gaf61979977343e39f21d6ea0b22167514>
clang_getIncludedFile :: MonadIO m => CXCursor -> m CXFile
clang_getIncludedFile cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      wrap_getIncludedFile cursor'

-- | If cursor refers to a variable declaration and it has initializer returns
-- cursor referring to the initializer otherwise return null cursor.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#ga74690016573b854df29f33b477872e7d>
clang_Cursor_getVarDeclInitializer :: MonadIO m => CXCursor -> m CXCursor
clang_Cursor_getVarDeclInitializer cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_Cursor_getVarDeclInitializer cursor'

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
     MonadIO m
  => CXCursor
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
  -> m Bool
     -- ^ 'True' if the traversal was terminated prematurely by the visitor
     -- returning 'CXChildVisit_Break'.
clang_visitChildren root visitor = liftIO $ do
    -- reference cell for a possible exception thrown by 'visitor'.
    eRef <- newIORef Nothing

    visitor' <- mkCursorVisitor $ \current parent -> do
      current' <- CXCursor <$> copyToHaskellHeap current
      parent'  <- CXCursor <$> copyToHaskellHeap parent
      try (visitor current' parent') >>= \case
        Left exc -> do
          writeIORef eRef (Just (exc :: SomeException))
          return (simpleEnum CXChildVisit_Break)
        Right x -> return x

    res <- onHaskellHeap root $ \parent' ->
      (/= 0) <$> wrap_visitChildren parent' visitor'

    readIORef eRef >>= \case
      Nothing  -> return res
      Just exc -> throwIO exc

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
clang_getCursorDisplayName :: MonadIO m => CXCursor -> m Text
clang_getCursorDisplayName cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      preallocate_$ wrap_getCursorDisplayName cursor'

-- | Retrieve a name for the entity referenced by this cursor.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#gaad1c9b2a1c5ef96cebdbc62f1671c763>
clang_getCursorSpelling :: MonadIO m => CXCursor -> m Text
clang_getCursorSpelling cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      preallocate_$ wrap_getCursorSpelling cursor'

-- | For a cursor that is a reference, retrieve a cursor representing the entity
-- that it references.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#gabf059155921552e19fc2abed5b4ff73a>
clang_getCursorReferenced :: MonadIO m => CXCursor -> m CXCursor
clang_getCursorReferenced cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_getCursorReferenced cursor'

-- | For a cursor that is either a reference to or a declaration of some entity,
-- retrieve a cursor that describes the definition of that entity.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#gafcfbec461e561bf13f1e8540bbbd655b>
clang_getCursorDefinition :: MonadIO m => CXCursor -> m CXCursor
clang_getCursorDefinition cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_getCursorDefinition cursor'

-- |  Retrieve the canonical cursor corresponding to the given cursor.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#gac802826668be9fd40a017523cc7d24fe>
clang_getCanonicalCursor :: MonadIO m => CXCursor -> m CXCursor
clang_getCanonicalCursor cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_getCanonicalCursor cursor'

-- | Given a cursor that represents a declaration, return the associated comment
-- text, including comment markers.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#ga32905a8b1858e67cf5d28b7ad7150779>
clang_Cursor_getRawCommentText :: MonadIO m => CXCursor -> m Text
clang_Cursor_getRawCommentText cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      preallocate_$ wrap_Cursor_getRawCommentText cursor'

-- | Given a cursor that represents a documentable entity (e.g., declaration),
-- return the associated brief comment.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#ga6b5282b915d457d728434c0651ea0b8b>
clang_Cursor_getBriefCommentText :: MonadIO m => CXCursor -> m Text
clang_Cursor_getBriefCommentText cursor = liftIO $
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
     MonadIO m
  => CXCursor
  -> CUInt
  -- ^ @pieceIndex@
  --
  -- The index of the spelling name piece. If this is greater than the actual
  -- number of pieces, it will return 'Nothing'.
  -> CUInt
  -- ^ @options@
  --
  -- Reserved.
  -> m (Maybe CXSourceRange)
clang_Cursor_getSpellingNameRange cursor pieceIndex options = liftIO $
    onHaskellHeap cursor $ \cursor' -> do
      -- We don't normally do multiple calls in the low-level bindings. However,
      -- the docs for @clang_Cursor_getSpellingNameRange@ claim that it returns
      -- NULL when @pieceIndex@ is out of range, but that is impossible: it does
      -- not return a pointer. We therefore do a \"null range\" check instead.
      result :: CXSourceRange <- preallocate_ $
        wrap_Cursor_getSpellingNameRange cursor' pieceIndex options
      isNull <- clang_Range_isNull result
      return $ if isNull then Nothing else Just result

-- | Determine whether the declaration pointed to by this cursor is also a
-- definition of that entity.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#ga6ad05634a73e693217088eaa693f0010>
clang_isCursorDefinition :: MonadIO m => CXCursor -> m Bool
clang_isCursorDefinition cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      cToBool <$> wrap_isCursorDefinition cursor'

-- | Retrieve the default policy for the cursor.
--
-- The policy should be released after use with clang_PrintingPolicy_dispose.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#gaae83c013276d1fff6475566a23d9fffd>
clang_getCursorPrintingPolicy :: MonadIO m => CXCursor -> m CXPrintingPolicy
clang_getCursorPrintingPolicy cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      wrap_getCursorPrintingPolicy cursor'

-- | Pretty print declarations.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#gab9d561cc237ce0d8bfbab80cdd5be216>
clang_getCursorPrettyPrinted :: MonadIO m => CXCursor -> CXPrintingPolicy -> m Text
clang_getCursorPrettyPrinted cursor pol = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_getCursorPrettyPrinted cursor' pol

-- | Release a printing policy.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#ga81b2a9cac2b0ad4da7086c7fd3d4256f>
clang_PrintingPolicy_dispose :: MonadIO m => CXPrintingPolicy -> m ()
clang_PrintingPolicy_dispose pol = liftIO $ nowrapper_PrintingPolicy_dispose pol

{-------------------------------------------------------------------------------
  Type information for CXCursors

  <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html>
-------------------------------------------------------------------------------}

-- | The type of an element in the abstract syntax tree.
--
-- <https://clang.llvm.org/doxygen/structCXType.html>
newtype CXType = CXType (OnHaskellHeap CXType_)
  deriving newtype (LivesOnHaskellHeap, Preallocate, Show)

instance Eq CXType where
  x == y = unsafeDupablePerformIO $ clang_equalTypes x y

instance Ord CXType where
    compare x y = compare 0 $ unsafeDupablePerformIO $ clang_compareTypes x y

foreign import capi unsafe "clang_wrappers.h wrap_cxtKind"
  wrap_cxtKind :: R CXType_ -> IO (SimpleEnum CXTypeKind)

foreign import capi unsafe "clang_wrappers.h"
  wrap_compareTypes :: R CXType_ -> R CXType_ -> IO CInt

foreign import capi unsafe "clang_wrappers.h"
  wrap_getUnqualifiedType :: R CXType_ -> W CXType_ -> IO ()

foreign import capi unsafe "clang_wrappers.h"
  wrap_Cursor_getStorageClass :: R CXCursor_ -> IO (SimpleEnum CX_StorageClass)

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
clang_getCursorType :: MonadIO m => CXCursor -> m CXType
clang_getCursorType cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_getCursorType cursor'

-- | Retrieve the spelling of a given CXTypeKind.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga6bd7b366d998fc67f4178236398d0666>
clang_getTypeKindSpelling :: MonadIO m => SimpleEnum CXTypeKind -> m Text
clang_getTypeKindSpelling kind = liftIO $
    preallocate_$ wrap_getTypeKindSpelling kind

-- | Pretty-print the underlying type using the rules of the language of the
-- translation unit from which it came.
--
-- Throws 'CallFailed' if the type is invalid.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gac9d37f61bede521d4f42a6553bcbc09f>
clang_getTypeSpelling :: (MonadIO m, HasCallStack) => CXType -> m Text
clang_getTypeSpelling typ = liftIO $ ensure (not . Text.null) $
     onHaskellHeap typ $ \typ' ->
       preallocate_$ wrap_getTypeSpelling typ'

-- | Retrieve the underlying type of a typedef declaration.
--
-- Throws 'CallFailed' if the cursor does not reference a typedef declaration.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga8de899fc18dc859b6fe3b97309f4fd52>
clang_getTypedefDeclUnderlyingType ::
     (MonadIO m, HasCallStack)
  => CXCursor -> m CXType
clang_getTypedefDeclUnderlyingType cursor = liftIO $ ensureValidType $
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_getTypedefDeclUnderlyingType cursor'

-- | Retrieve the integer type of an enum declaration.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga0f5f950bee4e1828b51a41f0eaa951c4>
clang_getEnumDeclIntegerType ::
     (MonadIO m, HasCallStack)
  => CXCursor -> m CXType
clang_getEnumDeclIntegerType cursor = liftIO $ ensureValidType $
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_getEnumDeclIntegerType cursor'

-- | Determine if the cursor specifies a record member that is a bit-field.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga750705f6b418b25ca00495b7392c740d>
clang_Cursor_isBitField :: MonadIO m => CXCursor -> m Bool
clang_Cursor_isBitField cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      cToBool <$> wrap_Cursor_isBitField cursor'

-- | Return the bit width of a bit-field declaration as an iteger.
--
-- If the cursor does not reference a bit-field, or if the bit-field's width
-- expression cannot be evaluated, -1 is returned.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga80bbb872dde5b2f26964081338108f91>
clang_getFieldDeclBitWidth :: MonadIO m => CXCursor -> m CInt
clang_getFieldDeclBitWidth cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      wrap_getFieldDeclBitWidth cursor'

-- | For pointer types, returns the type of the pointee.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gaafa3eb34932d8da1358d50ed949ff3ee>
clang_getPointeeType :: MonadIO m => CXType -> m CXType
clang_getPointeeType typ = liftIO $
    onHaskellHeap typ $ \typ' ->
      preallocate_ $ wrap_getPointeeType typ'

-- | Return the element type of an array, complex, or vector type.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gab35027c8bc48fab25f7698a415c93922>
clang_getElementType :: MonadIO m => CXType -> m CXType
clang_getElementType typ = liftIO $
    onHaskellHeap typ $ \typ' ->
      preallocate_ $ wrap_getElementType typ'

-- | Return the element type of an array type.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga718591f4b07d9d4861557a3ed8b29713>
clang_getArrayElementType :: MonadIO m => CXType -> m CXType
clang_getArrayElementType typ = liftIO $
    onHaskellHeap typ $ \typ' ->
      preallocate_ $ wrap_getArrayElementType typ'

-- | Return the array size of a constant array.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga91521260817054f153b5f1295056192d>
clang_getArraySize :: (MonadIO m, HasCallStack) => CXType -> m CLLong
clang_getArraySize typ = liftIO $
    onHaskellHeap typ $ \typ' -> ensureNotInRange @CXTypeLayoutError $
      wrap_getArraySize typ'

-- | Return the size of a type in bytes as per @C++[expr.sizeof]@ standard.
--
-- Throws 'CallFailed' with 'CXTypeLayoutError' on error.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga027abe334546e80931905f31399d0a8b>
clang_Type_getSizeOf :: (MonadIO m, HasCallStack) => CXType -> m CLLong
clang_Type_getSizeOf typ = liftIO $
    onHaskellHeap typ $ \typ' -> ensureNotInRange @CXTypeLayoutError $
      wrap_Type_getSizeOf typ'

-- | Return the alignment of a type in bytes as per C++[expr.alignof] standard.
--
-- Throws 'CallFailed' with 'CXTypeLayoutError' on error.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gaee56de66c69ab5605fe47e7c52497e31>
clang_Type_getAlignOf :: (MonadIO m, HasCallStack) => CXType -> m CLLong
clang_Type_getAlignOf typ = liftIO $
    onHaskellHeap typ $ \typ' -> ensureNotInRange @CXTypeLayoutError $
      wrap_Type_getAlignOf typ'

-- | Determine if a typedef is 'transparent' tag.
--
-- A typedef is considered 'transparent' if it shares a name and spelling
-- location with its underlying tag type, as is the case with the @NS_ENUM@
-- macro.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga9ac4ecb0e84f25b9f05d54c67353eba0>
clang_Type_isTransparentTagTypedef :: MonadIO m => CXType -> m Bool
clang_Type_isTransparentTagTypedef typ = liftIO $
    onHaskellHeap typ $ \typ' ->
      cToBool <$> wrap_Type_isTransparentTagTypedef typ'

-- | Return the offset of the field represented by the Cursor.
--
-- Unit: bits
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gaa7e0f0ec320c645e971168ac39aa0cab>
clang_Cursor_getOffsetOfField :: MonadIO m => CXCursor -> m CLLong
clang_Cursor_getOffsetOfField cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      wrap_Cursor_getOffsetOfField cursor'

-- | Returns the storage class for a function or variable declaration.
--
-- Throws 'CallFailed' if the passed in Cursor is not a function or variable
-- declaration.
--
-- NOTE: Storage classes cannot be combined.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga230c7904f3878469d772f3e464b9c83d>
clang_Cursor_getStorageClass ::
     MonadIO m
  => CXCursor -> m (SimpleEnum CX_StorageClass)
clang_Cursor_getStorageClass cursor = liftIO $ ensure (/= coerceSimpleEnum 0) $
    onHaskellHeap cursor $ \cursor' ->
      wrap_Cursor_getStorageClass cursor'

-- | Determine whether the given cursor represents an anonymous tag or
-- namespace.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga6e0d2674d126fd43816ce3a80b592373>
clang_Cursor_isAnonymous :: MonadIO m => CXCursor -> m Bool
clang_Cursor_isAnonymous cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      cToBool <$> wrap_Cursor_isAnonymous cursor'

-- | Determine whether the given cursor represents an anonymous record declaration.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga59aaf3b8329a35e400ee3735229a8cb6>
clang_Cursor_isAnonymousRecordDecl :: MonadIO m => CXCursor -> m Bool
clang_Cursor_isAnonymousRecordDecl cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      cToBool <$> wrap_Cursor_isAnonymousRecordDecl cursor'

-- | Retrieve the integer value of an enum constant declaration as a signed long
-- long.
--
-- Throws 'CallFailed' if the cursor does not reference an enum constant
-- declaration.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga6b8585818420e7512feb4c9d209b4f4d>
clang_getEnumConstantDeclValue ::
     (MonadIO m, HasCallStack)
  => CXCursor -> m CLLong
clang_getEnumConstantDeclValue cursor = liftIO $ do
    -- The @libclang@ docs state:
    --
    -- > If the cursor does not reference an enum constant declaration,
    -- > LLONG_MIN is returned. Since this is also potentially a valid constant
    -- > value, the kind of the cursor must be verified before calling this
    -- > function.
    cursorKind <- clang_getCursorKind cursor
    unless (cursorKind == simpleEnum CXCursor_EnumConstantDecl) $
      callFailedShow cursorKind

    onHaskellHeap cursor $ \cursor' ->
      wrap_getEnumConstantDeclValue cursor'

-- | Determine whether two CXTypes represent the same type.
clang_equalTypes :: MonadIO m => CXType -> CXType -> m Bool
clang_equalTypes a b = liftIO $
    onHaskellHeap a $ \a' ->
    onHaskellHeap b $ \b' ->
      cToBool <$> wrap_equalTypes a' b'

clang_compareTypes :: MonadIO m => CXType -> CXType -> m CInt
clang_compareTypes a b = liftIO $
    onHaskellHeap a $ \a' ->
    onHaskellHeap b $ \b' ->
      wrap_compareTypes a' b'

-- | Return the canonical type for a CXType.
--
-- Clang's type system explicitly models typedefs and all the ways a specific
-- type can be represented. The canonical type is the underlying type with all
-- the "sugar" removed. For example, if 'T' is a typedef for 'int', the
-- canonical type for 'T' would be 'int'.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gaa9815d77adc6823c58be0a0e32010f8c>
clang_getCanonicalType :: MonadIO m => CXType -> m CXType
clang_getCanonicalType typ = liftIO $
    onHaskellHeap typ $ \typ' ->
      preallocate_ $ wrap_getCanonicalType typ'

-- | Returns the typedef name of the given type.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga7b8e66707c7f27550acfc2daeec527ed>
clang_getTypedefName :: MonadIO m => CXType -> m Text
clang_getTypedefName arg = liftIO $
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
clang_getUnqualifiedType :: MonadIO m => CXType -> m CXType
clang_getUnqualifiedType typ = liftIO $ do
    -- clang_getUnqualifiedType was added in Clang 16
    requireClangVersion (16,0,0)
    -- clang_getUnqualifiedType segfaults when CT is invalid
    case fromSimpleEnum (cxtKind typ) of
      e@Left{}                 -> callFailedShow e
      e@(Right CXType_Invalid) -> callFailedShow e
      Right{}                  -> pure ()
    onHaskellHeap typ $ \typ' ->
      preallocate_ $ wrap_getUnqualifiedType typ'

-- | Determine whether a 'CXType' has the @const@ qualifier set, without
-- looking through typedefs that may have added "const" at a different level.
--
-- See also 'clang_getCanonicalType'.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga8c3f8029254d5862bcd595d6c8778e5b>
clang_isConstQualifiedType :: MonadIO m => CXType -> m Bool
clang_isConstQualifiedType typ = liftIO $ do
    onHaskellHeap typ $ \typ' ->
      cToBool <$> wrap_isConstQualifiedType typ'

-- | Determine whether a 'CXType' has the @volatile@ qualifier set, without
-- looking through typedefs that may have added "volatile" at a different level.
--
-- See also 'clang_getCanonicalType'.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gaac0ac93cded7d1e5c60f539daaed13ec>
clang_isVolatileQualifiedType :: MonadIO m => CXType -> m Bool
clang_isVolatileQualifiedType typ = liftIO $ do
    onHaskellHeap typ $ \typ' ->
      cToBool <$> wrap_isVolatileQualifiedType typ'

-- | Determine whether a 'CXType' has the @restrict@ qualifier set, without
-- looking through typedefs that may have added "restrict" at a different level.
--
-- See also 'clang_getCanonicalType'.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga12375c30c12b0c3ede87492605db1d0c>
clang_isRestrictQualifiedType :: MonadIO m => CXType -> m Bool
clang_isRestrictQualifiedType typ = liftIO $ do
    onHaskellHeap typ $ \typ' ->
      cToBool <$> wrap_isRestrictQualifiedType typ'

-- | Return the cursor for the declaration of the given type.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga0aad74ea93a2f5dea58fd6fc0db8aad4>
clang_getTypeDeclaration :: MonadIO m => CXType -> m CXCursor
clang_getTypeDeclaration typ = liftIO $
    onHaskellHeap typ $ \typ' ->
      preallocate_ $ wrap_getTypeDeclaration typ'

-- | Check if the CXType is a variadic function type
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga343b2463b0ed4b259739242cf26c3ae2>
clang_isFunctionTypeVariadic :: MonadIO m => CXType -> m Bool
clang_isFunctionTypeVariadic typ = liftIO $
    onHaskellHeap typ $ \typ' ->
      cToBool <$> wrap_isFunctionTypeVariadic typ'

-- | Retrieve the return type associated with a function type.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga39b4850746f39e17c6b8b4eef3154d85>
clang_getResultType :: MonadIO m => CXType -> m CXType
clang_getResultType typ = liftIO $
    onHaskellHeap typ $ \typ' ->
      preallocate_ $ wrap_getResultType typ'

-- | Retrieve the number of non-variadic parameters associated with a function type.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga705e1a4ed7c7595606fc30ed5d2a6b5a>
clang_getNumArgTypes :: MonadIO m => CXType -> m CInt
clang_getNumArgTypes typ = liftIO $
    onHaskellHeap typ $ \typ' ->
      wrap_getNumArgTypes typ'

-- | Retrieve the type of a parameter of a function type.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga67f60ba4831b1bfd90ab0c1c12adab27>
clang_getArgType :: MonadIO m => CXType -> CUInt -> m CXType
clang_getArgType typ n = liftIO $
    onHaskellHeap typ $ \typ' ->
      preallocate_ $ wrap_getArgType typ' n

-- | Retrieve the type named by the qualified-id.
--
-- Throws 'CallFailed' if a non-elaborated type is passed in.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gac6d90c2acdae77f75d8e8288658da463>
clang_Type_getNamedType :: (MonadIO m, HasCallStack) => CXType -> m CXType
clang_Type_getNamedType typ = liftIO $ ensureValidType $
    onHaskellHeap typ $ \typ' ->
      preallocate_ $ wrap_Type_getNamedType typ'

-- | Return the type that was modified by this attributed type.
--
-- Throws 'CallFailed' if the type is not an attributed type.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga6fc6ec9bfd9baada2d3fd6022d774675>
clang_Type_getModifiedType :: (MonadIO m, HasCallStack) => CXType -> m CXType
clang_Type_getModifiedType typ = liftIO $ ensureValidType $
    onHaskellHeap typ $ \typ' ->
      preallocate_ $ wrap_Type_getModifiedType typ'

-- | Gets the type contained by this atomic type.
--
-- Throws 'CallFailed' if a non-atomic type is passed in.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gae42d9886e0e221df03c4a518d9afb622>
clang_Type_getValueType :: (MonadIO m, HasCallStack) => CXType -> m CXType
clang_Type_getValueType typ = liftIO $ ensureValidType $
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
  deriving newtype (LivesOnHaskellHeap, Preallocate, Show)

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
clang_getCursorLocation :: MonadIO m => CXCursor -> m CXSourceLocation
clang_getCursorLocation cursor = liftIO $
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
clang_getCursorExtent :: MonadIO m => CXCursor -> m CXSourceRange
clang_getCursorExtent cursor = liftIO $
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
  nowrapper_getTokenKind :: CXToken -> IO (SimpleEnum CXTokenKind)

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
clang_getToken ::
     MonadIO m
  => CXTranslationUnit -> CXSourceLocation -> m (Maybe CXToken)
clang_getToken unit loc = liftIO $ checkNotNull $
    onHaskellHeap loc $ \loc' ->
      wrap_getToken unit loc'

-- | Determine the kind of the given token.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LEX.html#ga83f692a67fe4dbeea779f37c0a3b7f20>
clang_getTokenKind :: MonadIO m => CXToken -> m (SimpleEnum CXTokenKind)
clang_getTokenKind token = liftIO $ nowrapper_getTokenKind token

-- | Determine the spelling of the given token.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LEX.html#ga1033a25c9d2c59bcbdb23020de0bba2c>
clang_getTokenSpelling :: MonadIO m => CXTranslationUnit -> CXToken -> m Text
clang_getTokenSpelling unit token = liftIO $
    preallocate_ $ wrap_getTokenSpelling unit token

-- | Retrieve the source location of the given token.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LEX.html#ga76a721514acb4cc523e10a6913d88021>
clang_getTokenLocation ::
     MonadIO m
  => CXTranslationUnit -> CXToken -> m CXSourceLocation
clang_getTokenLocation unit token = liftIO $
    preallocate_ $ wrap_getTokenLocation unit token

-- | Retrieve a source range that covers the given token.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LEX.html#ga5acbc0a2a3c01aa44e1c5c5ccc4e328b>
clang_getTokenExtent ::
     MonadIO m
  => CXTranslationUnit -> CXToken -> m CXSourceRange
clang_getTokenExtent unit token = liftIO $
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

foreign import capi unsafe "clang-c/Index.h clang_disposeTokens"
  nowrapper_disposeTokens :: CXTranslationUnit -> CXTokenArray -> CUInt -> IO ()

-- | Tokenize the source code described by the given range into raw lexical
-- tokens.
--
-- Returns the array of tokens and the number of tokens in the array. The array
-- must be disposed using 'clang_disponseTokens' before the translation unit is
-- destroyed.
clang_tokenize ::
     MonadIO m
  => CXTranslationUnit
  -> CXSourceRange
  -> m (CXTokenArray, CUInt)
clang_tokenize unit range = liftIO $
    onHaskellHeap range $ \range' ->
      alloca $ \array ->
      alloca $ \numTokens -> do
        wrap_tokenize unit range' array numTokens
        (,) <$> peek array <*> peek numTokens

-- | Free the given set of tokens.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LEX.html#gac5266f6b5fee87c433b696437cab0d13>
clang_disposeTokens ::
     MonadIO m
  => CXTranslationUnit -> CXTokenArray -> CUInt -> m ()
clang_disposeTokens unit tokens numTokens = liftIO $
    nowrapper_disposeTokens unit tokens numTokens

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
     MonadIO m
  => CXTranslationUnit
  -> CXTokenArray  -- ^ Tokens to annotate
  -> CUInt         -- ^ Number of tokens in the array
  -> m CXCursorArray
clang_annotateTokens unit tokens numTokens = liftIO $ fmap CXCursorArray $
    preallocateArray (fromIntegral numTokens) $ \arr ->
      nowrapper_annotateTokens unit tokens numTokens arr

index_CXCursorArray :: MonadIO m => CXCursorArray -> CUInt -> m CXCursor
index_CXCursorArray (CXCursorArray arr) i = liftIO $
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
  deriving newtype (LivesOnHaskellHeap, Preallocate, Show)

foreign import capi unsafe "clang_wrappers.h wrap_getRangeStart"
  wrap_getRangeStart :: R CXSourceRange_ -> W CXSourceLocation_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_getRangeEnd"
  wrap_getRangeEnd :: R CXSourceRange_ -> W CXSourceLocation_ -> IO ()

foreign import capi unsafe "clang_wrappers.h wrap_Range_isNull"
  wrap_Range_isNull :: R CXSourceRange_ -> IO CInt

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
clang_getRangeStart :: MonadIO m => CXSourceRange -> m CXSourceLocation
clang_getRangeStart range = liftIO $
    onHaskellHeap range $ \range' ->
      preallocate_ $ wrap_getRangeStart range'

-- | Retrieve a source location representing the last character within a source
-- range.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#gacdb7d3c2b77a06bcc2e83bde3e14c3c0>
clang_getRangeEnd :: MonadIO m => CXSourceRange -> m CXSourceLocation
clang_getRangeEnd range = liftIO $
    onHaskellHeap range $ \range' ->
      preallocate_ $ wrap_getRangeEnd range'

-- | Check if range is null
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#ga39213a93703e84c0accdba1f618d7fbb>
clang_Range_isNull :: MonadIO m => CXSourceRange -> m Bool
clang_Range_isNull range = liftIO $
    onHaskellHeap range $ \range' ->
      cToBool <$> wrap_Range_isNull range'

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
     MonadIO m
  => CXSourceLocation
  -> m (CXFile, CUInt, CUInt, CUInt)
clang_getExpansionLocation location = liftIO $
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
clang_getPresumedLocation ::
     MonadIO m
  => CXSourceLocation -> m (Text, CUInt, CUInt)
clang_getPresumedLocation location = liftIO $
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
     MonadIO m
  => CXSourceLocation
  -> m (CXFile, CUInt, CUInt, CUInt)
clang_getSpellingLocation location = liftIO $
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
     MonadIO m
  => CXSourceLocation
  -> m (CXFile, CUInt, CUInt, CUInt)
clang_getFileLocation location = liftIO $
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
     MonadIO m
  => CXTranslationUnit
  -> CXFile
  -> CUInt   -- ^ Line
  -> CUInt   -- ^ Column
  -> m CXSourceLocation
clang_getLocation unit file line col = liftIO $
    preallocate_ $ wrap_getLocation unit file line col

-- | Retrieve a source range given the beginning and ending source locations.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#ga4e2b6d439f72fdee12c2e4dcf4ff1e2f>
clang_getRange ::
     MonadIO m
  => CXSourceLocation -> CXSourceLocation -> m CXSourceRange
clang_getRange begin end = liftIO $
    onHaskellHeap begin $ \begin' ->
    onHaskellHeap end   $ \end' ->
      preallocate_ $ wrap_getRange begin' end'

-- | Retrieve a file handle within the given translation unit.
--
-- Returns the file handle for the named file in the translation unit.
-- Throws 'CallFailed' if the file was not a part of this translation unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#gaa0554e2ea48ecd217a29314d3cbd2085>
clang_getFile ::
     (MonadIO m, HasCallStack)
  => CXTranslationUnit -> Text -> m CXFile
clang_getFile unit file = liftIO $ ensureNotNull' $
    withCString (Text.unpack file) $ \file' ->
      nowrapper_getFile unit file'
  where
    ensureNotNull' :: IO CXFile -> IO CXFile
    ensureNotNull' call = do
        x <- call
        if not (isNullPtr x)
          then return x
          else callFailed $ concat [
                   show file
                 , " is not a part of this translation unit"
                 ]

-- | Check if the given source location is in the main file of the corresponding
-- translation unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#gacb4ca7b858d66f0205797ae84cc4e8f2>
clang_Location_isFromMainFile :: MonadIO m => CXSourceLocation -> m Bool
clang_Location_isFromMainFile location = liftIO $
    onHaskellHeap location $ \location' ->
      cToBool <$> wrap_Location_isFromMainFile location'

{-------------------------------------------------------------------------------
  File manipulation routines

  <https://clang.llvm.org/doxygen/group__CINDEX__FILES.html>
-------------------------------------------------------------------------------}

-- | Retrieve the complete file and path name of the given file.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__FILES.html#ga626ff6335ab1e0a2b8c8823301225690>
clang_getFileName :: MonadIO m => CXFile -> m Text
clang_getFileName file = liftIO $ preallocate_$ wrap_getFileName file

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

foreign import capi "clang_wrappers.h clang_breakpoint"
  nowrapper_breakpoint :: IO ()

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
clang_breakpoint :: MonadIO m => m ()
clang_breakpoint = liftIO $ nowrapper_breakpoint

{-------------------------------------------------------------------------------
  Rewrite API
-------------------------------------------------------------------------------}

-- | An opaque type representing a Clang rewriter
--
-- <https://clang.llvm.org/doxygen/Rewrite_8h.html>
newtype {-# CType "CXRewriter" #-} CXRewriter = CXRewriter (Ptr ())
  deriving stock (Show)

foreign import capi unsafe "rewrite_wrappers.h clang_CXRewriter_create"
  nowrapper_CXRewriter_create :: CXTranslationUnit -> IO CXRewriter

foreign import capi unsafe "rewrite_wrappers.h wrap_CXRewriter_insertTextBefore"
  wrap_CXRewriter_insertTextBefore ::
       CXRewriter
    -> R CXSourceLocation_
    -> CString
    -> IO ()

foreign import capi unsafe "rewrite_wrappers.h clang_CXRewriter_writeMainFileToStdOut"
  nowrapper_CXRewriter_writeMainFileToStdOut :: CXRewriter -> IO ()

foreign import capi unsafe "rewrite_wrappers.h clang_CXRewriter_dispose"
  nowrapper_CXRewriter_dispose :: CXRewriter -> IO ()

clang_CXRewriter_create :: MonadIO m => CXTranslationUnit -> m CXRewriter
clang_CXRewriter_create = liftIO . nowrapper_CXRewriter_create

clang_CXRewriter_insertTextBefore ::
     MonadIO m
  => CXRewriter
  -> CXSourceLocation
  -> Text
  -> m ()
clang_CXRewriter_insertTextBefore rewriter loc text = liftIO $
    onHaskellHeap loc $ \loc' ->
      withCString (Text.unpack text) $
        wrap_CXRewriter_insertTextBefore rewriter loc'

clang_CXRewriter_writeMainFileToStdOut :: MonadIO m => CXRewriter -> m ()
clang_CXRewriter_writeMainFileToStdOut =
    liftIO . nowrapper_CXRewriter_writeMainFileToStdOut

clang_CXRewriter_dispose :: MonadIO m => CXRewriter -> m ()
clang_CXRewriter_dispose = liftIO . nowrapper_CXRewriter_dispose

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

-- | Memoised call to 'clang_getNullCursor'.
nullCursor :: CXCursor
nullCursor = unsafePerformIO clang_getNullCursor
{-# NOINLINE nullCursor #-}
