{-# LANGUAGE CApiFFI #-}

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
-- Despite the low-level nature, these bindings should be useable without
-- imports of @Foreign.*@.
--
-- Guidelines:
--
-- * The goal of this module is not to be a complete set of bindings for all of
--   @libclang@, but rather only to the parts that we need. We do include
--   documentation.
-- * Names are chosen to be as close as possible to the C API.
-- * Structs are left opaque, with provided accessors where necessary. An
--   accessor for a field @field@ of a type @CXFooBar@ is called @cxfbField@.
-- * For functions that take structs or return structs by value, we use our own
--   wrappers from @cbits/clang_wrappers.h@.
-- * In cases where we are responsible for calling @free@ (or some other
--   finalizer), we use 'ForeignPtr' rather than 'Ptr' in argument position.
--
-- The sections in this module and in the export list correspond to
-- <https://clang.llvm.org/doxygen/group__CINDEX.html>, with the exception of
-- section "Physical source locations", which is for some reason not mentioned
-- there.
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/80> Ideally we would
-- bootstrap this (generate it using @hs-bindgen@ itself).
module HsBindgen.C.Clang (
    -- * Top-level
    CXIndex
  , DisplayDiagnostics(..)
  , clang_createIndex
    -- * Translation unit manipulation
  , CXTranslationUnit
  , CXUnsavedFile
  , CXTranslationUnit_Flags
  , CXTranslationUnit_Flag(..)
  , clang_parseTranslationUnit
    -- * Cursor manipulations
  , CXCursor
  , clang_getTranslationUnitCursor
  , clang_equalCursors
    -- * Traversing the AST with cursors
  , CXChildVisitResult(..)
  , CXCursorVisitor
  , clang_visitChildren
    -- * Cross-referencing in the AST
  , CXString
  , clang_getCursorDisplayName
  , clang_getCursorSpelling
    -- * Type information for CXCursors
  , CXTypeKind(..)
  , CXType
  , cxtKind
  , clang_getCursorType
  , clang_getTypeKindSpelling
  , clang_getTypeSpelling
  , clang_getPointeeType
  , clang_Type_getSizeOf
  , clang_Type_getAlignOf
    -- * Mapping between cursors and source code
  , CXSourceRange
  , clang_getCursorExtent
    -- * Physical source locations
  , CXSourceLocation
  , clang_getRangeStart
  , clang_getRangeEnd
  , clang_getExpansionLocation
    -- * Error results (exceptions)
  , CallFailed(..)
  ) where

import Control.Exception
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Foreign
import Foreign.C
import Foreign.Concurrent qualified as Concurrent
import GHC.Stack
import System.IO.Unsafe (unsafePerformIO)

import HsBindgen.C.Clang.Enums
import HsBindgen.C.Clang.Instances ()
import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Top-level

  <https://clang.llvm.org/doxygen/group__CINDEX.html>
-------------------------------------------------------------------------------}

-- | An "index" that consists of a set of translation units that would typically
-- be linked together into an executable or library.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#gae039c2574bfd75774ca7a9a3e55910cb>
data CXIndex

foreign import capi unsafe "clang-c/Index.h clang_createIndex"
  clang_createIndex' ::
       CInt
       -- ^ @excludeDeclarationsFromPCH@
    -> CInt
       -- ^ @displayDiagnostics@
    -> IO (Ptr CXIndex)

data DisplayDiagnostics =
    DisplayDiagnostics
  | DontDisplayDiagnostics

-- | Provides a shared context for creating translation units.
--
-- /NOTE/: We are not planning to support precompiled headers, so we omit the
-- first argument (@excludeDeclarationsFromPCH@).
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#ga51eb9b38c18743bf2d824c6230e61f93>
clang_createIndex ::
     DisplayDiagnostics
  -> IO (Ptr CXIndex)
clang_createIndex diagnostics =
    clang_createIndex' 0 diagnostics'
  where
    diagnostics' :: CInt
    diagnostics' =
        case diagnostics of
          DisplayDiagnostics     -> 1
          DontDisplayDiagnostics -> 0

{-------------------------------------------------------------------------------
  Definition of 'CXTranslationUnit_Flags'
-------------------------------------------------------------------------------}

-- | Flags that control the creation of translation units.
--
-- The enumerators in this enumeration type are meant to be bitwise ORed
-- together to specify which options should be used when constructing the
-- translation unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TRANSLATION__UNIT.html#gab1e4965c1ebe8e41d71e90203a723fe9>
type CXTranslationUnit_Flags = BitfieldEnum CXTranslationUnit_Flag

{-------------------------------------------------------------------------------
  Translation unit manipulation

  <https://clang.llvm.org/doxygen/group__CINDEX__TRANSLATION__UNIT.html>
-------------------------------------------------------------------------------}

-- | A single translation unit, which resides in an index.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#gacdb7815736ca709ce9a5e1ec2b7e16ac>
data CXTranslationUnit

-- | Provides the contents of a file that has not yet been saved to disk.
--
-- Each 'CXUnsavedFile' instance provides the name of a file on the system along
-- with the current contents of that file that have not yet been saved to disk.
--
-- <https://clang.llvm.org/doxygen/structCXUnsavedFile.html>
data CXUnsavedFile

-- We use @ccall@ rather than @capi@ to avoid problems with the
-- @const char *const *@ type
-- (see also <https://gitlab.haskell.org/ghc/ghc/-/issues/22043>).
foreign import ccall unsafe "clang-c/Index.h clang_parseTranslationUnit"
  clang_parseTranslationUnit' ::
       Ptr CXIndex
    -> CString
    -> Ptr CString
    -> CInt
    -> Ptr CXUnsavedFile
    -> CUInt
    -> CXTranslationUnit_Flags
    -> IO (Ptr CXTranslationUnit)

-- | Same as 'clang_parseTranslationUnit2', but returns the 'CXTranslationUnit'
-- instead of an error code.
--
-- In case of an error this routine throws an exception.
--
-- /NOTE/: We omit arugments for unsaved files; we have no plans for supporting
-- a workflow where the C headers are being edited, and the bindings dynamically
-- updated.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TRANSLATION__UNIT.html#ga2baf83f8c3299788234c8bce55e4472e>
clang_parseTranslationUnit ::
     HasCallStack
  => Ptr CXIndex
     -- ^ @CIdx@
  -> FilePath
     -- ^ @source_filename@
  -> [String]
     -- ^ @command_line_args@
  -> CXTranslationUnit_Flags
     -- ^ @options@
  -> IO (Ptr CXTranslationUnit)
clang_parseTranslationUnit cIdx src args options =
    withCString  src  $ \src' ->
    withCStrings args $ \args' numArgs -> ensureNotNull $
      clang_parseTranslationUnit' cIdx src' args' numArgs nullPtr 0 options

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
-- For now we keep this type abstract.
--
-- <https://clang.llvm.org/doxygen/structCXCursor.html>
data CXCursor

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getTranslationUnitCursor"
  clang_getTranslationUnitCursor' ::
       Ptr CXTranslationUnit
    -> IO (Ptr CXCursor)

-- | Retrieve the cursor that represents the given translation unit.
--
-- The translation unit cursor can be used to start traversing the various
-- declarations within the given translation unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#gaec6e69127920785e74e4a517423f4391>
clang_getTranslationUnitCursor ::
     Ptr CXTranslationUnit
  -> IO (ForeignPtr CXCursor)
clang_getTranslationUnitCursor unit = attachFinalizer =<<
    clang_getTranslationUnitCursor' unit

foreign import capi unsafe "clang_wrappers.h wrap_equalCursors"
  clang_equalCursors' ::
       Ptr CXCursor
    -> Ptr CXCursor
    -> IO CUInt

-- | Determine whether two cursors are equivalent.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#ga98df58f09878710b983b6f3f60f0cba3>
clang_equalCursors ::
     ForeignPtr CXCursor
  -> ForeignPtr CXCursor
  -> IO Bool
clang_equalCursors a b =
    withForeignPtr a $ \a' ->
    withForeignPtr b $ \b' ->
      (/= 0) <$> clang_equalCursors' a' b'

{-------------------------------------------------------------------------------
  Traversing the AST with cursors

  <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__TRAVERSAL.html>
-------------------------------------------------------------------------------}

-- | Visitor invoked for each cursor found by a traversal.
--
-- This visitor function will be invoked for each cursor found by
-- 'clang_visitCursorChildren'.
--
-- /NOTE/: The C type @CXCursorVisitor@ accepts a /third/ argument, which is a
-- pointer to some arbitrary piece of data. This is not necessary in Haskell:
-- visitors can have arbitrary data in their closure.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__TRAVERSAL.html#gabf842c9ee20048b596eb9dfe94bb1570>
type CXCursorVisitor =
     Ptr CXCursor
     -- ^ The cursor being visited.
  -> Ptr CXCursor
     -- ^ The parent visitor for that cursor.
  -> IO (SimpleEnum CXChildVisitResult)
     -- ^ The visitor should return one of the 'CXChildVisitResult' values to
     -- direct 'clang_visitCursorChildren'.

foreign import ccall "wrapper"
  mkCursorVisitor :: CXCursorVisitor -> IO (FunPtr CXCursorVisitor)

-- | See 'clang_visitChildren' for docs
--
-- /NOTE/: This is marked @safe@ rather than @unsafe@ as this calls back into
-- Haskell.
foreign import capi safe "clang_wrappers.h wrap_malloc_visitChildren"
  clang_visitChildren' ::
       Ptr CXCursor
    -> FunPtr CXCursorVisitor
    -> IO CUInt

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
     ForeignPtr CXCursor
     -- ^ @parent@
     --
     -- The cursor whose child may be visited. All kinds of cursors can be
     -- visited, including invalid cursors (which, by definition, have no
     -- children).
  -> (    ForeignPtr CXCursor
       -> ForeignPtr CXCursor
       -> IO (SimpleEnum CXChildVisitResult)
     )
     -- ^ @visitor@
     --
     -- The visitor function that will be invoked for each child of parent.
     -- See 'CXCursorVisitor' for details.
     --
     -- /NOTE/: We omit the @client_data@ argument from @libclang@, as it is
     -- not needed in Haskell (the IO action can have arbitrary data in its
     -- closure).
  -> IO Bool
     -- ^ 'True' if the traversal was terminated prematurely by the visitor
     -- returning 'CXChildVisit_Break'.
clang_visitChildren root visitor = do
    visitor' <- mkCursorVisitor $ \current parent -> do
      current' <- attachFinalizer current
      parent'  <- attachFinalizer parent
      visitor current' parent'
    withForeignPtr root $ \parent' ->
      (/= 0) <$> clang_visitChildren' parent' visitor'

{-------------------------------------------------------------------------------
  Cross-referencing in the AST

  <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html>
-------------------------------------------------------------------------------}

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getCursorDisplayName"
  clang_getCursorDisplayName' ::
       Ptr CXCursor
    -> IO (Ptr CXString)

-- | Retrieve the display name for the entity referenced by this cursor.
--
-- The display name contains extra information that helps identify the cursor,
-- such as the parameters of a function or template or the arguments of a class
-- template specialization.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#gac3eba3224d109a956f9ef96fd4fe5c83>
clang_getCursorDisplayName ::
     ForeignPtr CXCursor
  -> IO Strict.ByteString
clang_getCursorDisplayName cursor =
    withForeignPtr cursor $ \cursor' -> packCXString =<<
      clang_getCursorDisplayName' cursor'

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getCursorSpelling"
  clang_getCursorSpelling' ::
       Ptr CXCursor
    -> IO (Ptr CXString)

-- | Retrieve a name for the entity referenced by this cursor.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#gaad1c9b2a1c5ef96cebdbc62f1671c763>
clang_getCursorSpelling ::
     ForeignPtr CXCursor
  -> IO Strict.ByteString
clang_getCursorSpelling cursor =
    withForeignPtr cursor $ \cursor' -> packCXString =<<
      clang_getCursorSpelling' cursor'

{-------------------------------------------------------------------------------
  Type information for CXCursors

  <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html>
-------------------------------------------------------------------------------}

-- | The type of an element in the abstract syntax tree.
--
-- <https://clang.llvm.org/doxygen/structCXType.html>
data CXType

foreign import capi unsafe "clang_wrappers.h wrap_cxtKind"
  cxtKind' ::
       Ptr CXType
    -> IO (SimpleEnum CXTypeKind)

cxtKind :: ForeignPtr CXType -> SimpleEnum CXTypeKind
cxtKind typ = unsafePerformIO $
    withForeignPtr typ $ \typ' ->
      cxtKind' typ'

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getCursorType"
  clang_getCursorType' ::
       Ptr CXCursor
    -> IO (Ptr CXType)

-- | Retrieve the type of a CXCursor (if any).
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gaae5702661bb1f2f93038051737de20f4>
clang_getCursorType :: ForeignPtr CXCursor -> IO (ForeignPtr CXType)
clang_getCursorType cursor =
    withForeignPtr cursor $ \cursor' -> attachFinalizer =<<
      clang_getCursorType' cursor'

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getTypeKindSpelling"
  clang_getTypeKindSpelling' ::
     SimpleEnum CXTypeKind
  -> IO (Ptr CXString)

-- | Retrieve the spelling of a given CXTypeKind.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga6bd7b366d998fc67f4178236398d0666>
clang_getTypeKindSpelling ::
     SimpleEnum CXTypeKind
  -> IO Strict.ByteString
clang_getTypeKindSpelling kind = packCXString =<<
    clang_getTypeKindSpelling' kind

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getTypeSpelling"
  clang_getTypeSpelling' ::
       Ptr CXType
    -> IO (Ptr CXString)

-- | Pretty-print the underlying type using the rules of the language of the
-- translation unit from which it came.
--
-- If the type is invalid, an empty string is returned.
clang_getTypeSpelling ::
     ForeignPtr CXType
  -> IO Strict.ByteString
clang_getTypeSpelling typ =
     withForeignPtr typ $ \typ' -> packCXString =<<
       clang_getTypeSpelling' typ'

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getPointeeType"
  clang_getPointeeType' ::
       Ptr CXType
    -> IO (Ptr CXType)

-- | For pointer types, returns the type of the pointee.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gaafa3eb34932d8da1358d50ed949ff3ee>
clang_getPointeeType ::
     ForeignPtr CXType
  -> IO (ForeignPtr CXType)
clang_getPointeeType typ =
    withForeignPtr typ $ \typ' -> attachFinalizer =<<
      clang_getPointeeType' typ'

foreign import capi unsafe "clang_wrappers.h wrap_Type_getSizeOf"
  clang_Type_getSizeOf' ::
       Ptr CXType
    -> IO CLLong

-- | Return the size of a type in bytes as per C++[expr.sizeof] standard.
--
-- May throw 'CXTypeLayoutException'.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga027abe334546e80931905f31399d0a8b>
clang_Type_getSizeOf ::
     ForeignPtr CXType
  -> IO CLLong
clang_Type_getSizeOf typ =
    withForeignPtr typ $ \typ' -> ensure (>= 0) CXTypeLayoutException $
      clang_Type_getSizeOf' typ'

foreign import capi unsafe "clang_wrappers.h wrap_Type_getAlignOf"
  clang_getAlignOf' ::
       Ptr CXType
    -> IO CLLong

-- | Return the alignment of a type in bytes as per C++[expr.alignof] standard.
--
-- May throw 'CXTypeLayoutException'.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gaee56de66c69ab5605fe47e7c52497e31>
clang_Type_getAlignOf ::
     ForeignPtr CXType
  -> IO CLLong
clang_Type_getAlignOf typ =
    withForeignPtr typ $ \typ' -> ensure (>= 0) CXTypeLayoutException $
      clang_getAlignOf' typ'

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
data CXSourceRange

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getCursorExtent"
  clang_getCursorExtent' ::
       Ptr CXCursor
    -> IO (Ptr CXSourceRange)

-- | Retrieve the physical extent of the source construct referenced by the given cursor.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__SOURCE.html#ga79f6544534ab73c78a8494c4c0bc2840>
clang_getCursorExtent ::
     ForeignPtr CXCursor
  -> IO (ForeignPtr CXSourceRange)
clang_getCursorExtent cursor =
    withForeignPtr cursor $ \cursor' -> attachFinalizer =<<
      clang_getCursorExtent' cursor'

{-------------------------------------------------------------------------------
  Physical source locations
-------------------------------------------------------------------------------}

-- | Identifies a specific source location within a translation unit.
--
-- Use 'clang_getExpansionLocation' or 'clang_getSpellingLocation' to map a
-- source location to a particular file, line, and column.
--
-- <https://clang.llvm.org/doxygen/structCXSourceLocation.html>
data CXSourceLocation

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getRangeStart"
  clang_getRangeStart' ::
       Ptr CXSourceRange
    -> IO (Ptr CXSourceLocation)

-- | Retrieve a source location representing the first character within a source
-- range.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#gac2cc034e3965739c41662f6ada7ff248>
clang_getRangeStart ::
     ForeignPtr CXSourceRange
  -> IO (ForeignPtr CXSourceLocation)
clang_getRangeStart range =
    withForeignPtr range $ \range' -> attachFinalizer =<<
      clang_getRangeStart' range'

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getRangeEnd"
  clang_getRangeEnd' ::
       Ptr CXSourceRange
    -> IO (Ptr CXSourceLocation)

-- | Retrieve a source location representing the last character within a source
-- range.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#gacdb7d3c2b77a06bcc2e83bde3e14c3c0>
clang_getRangeEnd ::
     ForeignPtr CXSourceRange
  -> IO (ForeignPtr CXSourceLocation)
clang_getRangeEnd range =
    withForeignPtr range $ \range' -> attachFinalizer =<<
      clang_getRangeEnd' range'

-- | A particular source file that is part of a translation unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__FILES.html#gacfcea9c1239c916597e2e5b3e109215a>
data CXFile

foreign import capi "clang_wrappers.h wrap_getExpansionLocation"
  clang_getExpansionLocation' ::
       Ptr CXSourceLocation
       -- ^ the location within a source file that will be decomposed into its parts.
    -> Ptr (Ptr CXFile)
       -- ^ [out] if non-NULL, will be set to the file to which the given source location points.
    -> Ptr CUInt
       -- ^ [out] if non-NULL, will be set to the line to which the given source location points.
    -> Ptr CUInt
       -- ^ [out] if non-NULL, will be set to the column to which the given source location points.
    -> Ptr CUInt
       -- ^ [out] if non-NULL, will be set to the offset into the buffer to which the given source location points.
    -> IO ()

-- | Retrieve the file, line, column, and offset represented by the given source
-- location.
--
-- If the location refers into a macro expansion, retrieves the location of the
-- macro expansion.
--
-- Returns the file, line, column and offset into the buffer.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#gadee4bea0fa34550663e869f48550eb1f>
clang_getExpansionLocation ::
     ForeignPtr CXSourceLocation
  -> IO (Ptr CXFile, CUInt, CUInt, CUInt)
clang_getExpansionLocation location =
     withForeignPtr location $ \location' ->
       alloca $ \file ->
       alloca $ \line ->
       alloca $ \column ->
       alloca $ \offset -> do
         clang_getExpansionLocation' location' file line column offset
         (,,,) <$> peek file <*> peek line <*> peek column <*> peek offset

{-------------------------------------------------------------------------------
  Internal: String manipulation routines

  <https://clang.llvm.org/doxygen/group__CINDEX__STRING.html>
-------------------------------------------------------------------------------}

-- | A character string.
--
-- The 'CXString' type is used to return strings from the interface when the
-- ownership of that string might differ from one call to the next. Use
-- 'clang_getCString' to retrieve the string data and, once finished with the
-- string data, call 'clang_disposeString' to free the string.
--
-- <https://clang.llvm.org/doxygen/structCXString.html>
data CXString

-- | Retrieve the character data associated with the given string.
--
-- We use @ccall@ rather than @capi@ here to avoid compiler warning about
-- casting @const char *@ to @char *@ (we make a copy of the C string and then
-- do not use it again, so it's safe).
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__STRING.html#gabe1284209a3cd35c92e61a31e9459fe7>
foreign import ccall unsafe "clang_wrappers.h wrap_getCString"
  clang_getCString ::
       Ptr CXString
    -> IO CString

-- | Free the given string.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__STRING.html#gaeff715b329ded18188959fab3066048f>
foreign import capi unsafe "clang_wrappers.h wrap_disposeString"
  clang_disposeString ::
       Ptr CXString
    -> IO ()

-- | Pack 'CXString'
--
-- This is intended to be used in a similar way to 'attachFinalizer'.
--
-- The @libclang@ functions that return a @CXString@ do so by /value/; we
-- allocate this on the heap in our wrapper functions. Since we no longer need
-- this after packing, we free the pointer after packing.
packCXString :: Ptr CXString -> IO Strict.ByteString
packCXString str =
    bracket
      (clang_getCString str)
      (\_ -> clang_disposeString str >> free str) $
      BS.Strict.packCString

{-------------------------------------------------------------------------------
  Checking for error results

  We do the actual checks in this module, and export only the exception types.
-------------------------------------------------------------------------------}

data CallFailed = CallFailed Backtrace
  deriving stock (Show)
  deriving Exception via CollectedBacktrace CallFailed

-- | Ensure that a function did not return 'nullPtr' (indicating error)
ensureNotNull :: HasCallStack => IO (Ptr a) -> IO (Ptr a)
ensureNotNull call = do
    ptr <- call
    if ptr == nullPtr then do
      stack <- collectBacktrace
      throwIO $ CallFailed stack
    else
      return ptr

data CXTypeLayoutException =
    CXTypeLayoutException Backtrace CInt (Maybe CXTypeLayoutError)
  deriving stock (Show)
  deriving Exception via CollectedBacktrace CXTypeLayoutException

-- | Check that an (integral) result from @libclang@ function is not an error
ensure ::
     ( HasCallStack
     , IsSimpleEnum hs
     , Exception e
     , Integral c
     )
  => (c -> Bool)
  -> (Backtrace -> CInt -> Maybe hs -> e)
  -> IO c -> IO c
ensure p mkErr call = do
    c <- call
    if p c then
      return c
    else do
      stack <- collectBacktrace
      throwIO $ mkErr stack (fromIntegral c) (simpleFromC $ fromIntegral c)

{-------------------------------------------------------------------------------
  Internal: auxiliary
-------------------------------------------------------------------------------}

attachFinalizer :: Ptr a -> IO (ForeignPtr a)
attachFinalizer ptr = Concurrent.newForeignPtr ptr $ free ptr

-- | Extension of 'withCString' for multiple CStrings
withCStrings :: [String] -> (Ptr CString -> CInt -> IO r) -> IO r
withCStrings = \args k ->
    allocaArray (length args) $ \arr ->
      go args $ \args' -> do
        pokeArray arr args'
        k arr (fromIntegral $ length args)
  where
    go :: [String] -> ([CString] -> IO r) -> IO r
    go []     k = k []
    go (x:xs) k = withCString x  $ \x'  ->
                  go          xs $ \xs' ->
                  k (x' : xs')
