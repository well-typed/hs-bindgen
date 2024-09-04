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
--   wrappers from @cbits/clang_wrappers.h@.
--
-- * In cases where we are responsible for calling @free@ (or some other
--   finalizer), we use 'ForeignPtr' rather than 'Ptr' in argument position.
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
--    @wrap_foo@ or @wrap_malloc_foo@, and are imported as such.
--
--    The results of functions @wrap_malloc_foo@ are newly allocated, and must
--    be given a finalizer in the Haskell wrapper.
--
-- 2. For functions for which we don't need a C wrapper, we import the
--    @libclang@ function as @unwrapped_foo@.
--
-- 3. In the rare case that we can export the C function directly, we import it
--    simply as @clang_foo@.
--
-- /Note on pointers/: in the public API, all @libclang@ types are opaque.
-- Internally, they are all newtypes around either 'Ptr' or 'ForeignPtr',
-- depending on whether or not we own the value.
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/80> Ideally we would
-- bootstrap this (generate it using @hs-bindgen@ itself).
module HsBindgen.Clang.Core (
    -- * Top-level
    CXIndex
  , DisplayDiagnostics(..)
  , clang_createIndex
    -- * Translation unit manipulation
  , CXTranslationUnit
  , CXUnsavedFile
  , CXTranslationUnit_Flags
  , CXTranslationUnit_Flag(..)
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
    -- * Traversing the AST with cursors
  , CXChildVisitResult(..)
  , CXCursorVisitor
  , clang_visitChildren
    -- * Cross-referencing in the AST
  , CXString
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
  , clang_getPointeeType
  , clang_Type_getSizeOf
  , clang_Type_getAlignOf
  , clang_Type_isTransparentTagTypedef
  , clang_Cursor_isAnonymous
  , clang_getEnumConstantDeclValue
    -- * Mapping between cursors and source code
  , CXSourceRange
  , clang_getCursorLocation
  , clang_getCursorExtent
    -- * Token extraction and manipulation
  , CXToken
  , clang_getToken
  , clang_getTokenKind
  , clang_getTokenSpelling
  , clang_getTokenLocation
  , clang_getTokenExtent
    -- * Physical source locations
  , CXSourceLocation
  , clang_getRangeStart
  , clang_getRangeEnd
  , clang_getExpansionLocation
  , clang_getSpellingLocation
  , clang_Location_isFromMainFile
    -- * File manipulation routines
  , clang_getFileName
    -- * Exceptions
  , CallFailed(..)
    -- * Exported for the benefit of other bindings
  , CXCursor_(..)
  ) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Foreign
import Foreign.C
import GHC.Stack
import System.IO.Unsafe (unsafePerformIO)

import HsBindgen.Clang.Core.Enums
import HsBindgen.Clang.Core.Instances ()
import HsBindgen.Clang.Internal.Bindings
import HsBindgen.Clang.Internal.CXString
import HsBindgen.Clang.Internal.FFI
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
  deriving newtype (IsPointer)

foreign import capi unsafe "clang-c/Index.h clang_createIndex"
  nowrapper_clang_createIndex ::
       CInt -- ^ @excludeDeclarationsFromPCH@
    -> CInt -- ^ @displayDiagnostics@
    -> IO CXIndex

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
newtype CXTranslationUnit = CXTranslationUnit (Ptr ())
  deriving stock (Show)
  deriving newtype (IsPointer)

-- | Provides the contents of a file that has not yet been saved to disk.
--
-- Each 'CXUnsavedFile' instance provides the name of a file on the system along
-- with the current contents of that file that have not yet been saved to disk.
--
-- <https://clang.llvm.org/doxygen/structCXUnsavedFile.html>
newtype CXUnsavedFile = CXUnsavedFile (Ptr ())
  deriving stock (Show)
  deriving newtype (IsPointer)

-- | An opaque type representing target information for a given translation
-- unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#ga6b47552ab8c5d81387070a9b197cd3e2>
newtype {-# CType "CXTargetInfo" #-} CXTargetInfo = CXTargetInfo (Ptr ())
  deriving stock (Show)
  deriving newtype (IsPointer)

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
    -> CXTranslationUnit_Flags
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

foreign import capi "clang_wrappers.h wrap_malloc_TargetInfo_getTriple"
  wrap_malloc_TargetInfo_getTriple :: CXTargetInfo -> IO CXString

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
  => CXIndex                  -- ^ @CIdx@
  -> FilePath                 -- ^ @source_filename@
  -> [String]                 -- ^ @command_line_args@
  -> CXTranslationUnit_Flags  -- ^ @options@
  -> IO CXTranslationUnit
clang_parseTranslationUnit cIdx src args options =
    withCString  src  $ \src' ->
    withCStrings args $ \args' numArgs -> ensureNotNull $
      nowrapper_parseTranslationUnit cIdx src' args' numArgs mkNullPtr 0 options

-- | Get the normalized target triple as a string.
--
-- Throws 'CallFailed' on error.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TRANSLATION__UNIT.html#ga7ae67e3c8baf6a9852900f6529dce2d0>
clang_TargetInfo_getTriple :: HasCallStack => CXTargetInfo -> IO ByteString
clang_TargetInfo_getTriple info =
    ensure (not . BS.null) $
      packCXString =<< wrap_malloc_TargetInfo_getTriple info

{-------------------------------------------------------------------------------
  Cursor manipulations

  <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html>
-------------------------------------------------------------------------------}

newtype CXCursor_ = CXCursor_ (Ptr ())

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
newtype CXCursor = CXCursor (ForeignPtr CXCursor_)
  deriving IsForeignPtr via DeriveIsForeignPtr CXCursor_ CXCursor

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getTranslationUnitCursor"
  wrap_malloc_getTranslationUnitCursor :: CXTranslationUnit -> IO CXCursor_

foreign import capi unsafe "clang_wrappers.h wrap_equalCursors"
  nowrapper_equalCursors :: CXCursor_ -> CXCursor_ -> IO CUInt

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getCursorSemanticParent"
  wrap_malloc_getCursorSemanticParent :: CXCursor_ -> IO CXCursor_

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getCursorLexicalParent"
  wrap_malloc_getCursorLexicalParent :: CXCursor_ -> IO CXCursor_

foreign import capi unsafe "clang_wrappers.h wrap_getCursorKind"
  wrap_getCursorKind :: CXCursor_ -> IO (SimpleEnum CXCursorKind)

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getCursorKindSpelling"
  wrap_malloc_getCursorKindSpelling :: SimpleEnum CXCursorKind -> IO CXString

-- | Retrieve the cursor that represents the given translation unit.
--
-- The translation unit cursor can be used to start traversing the various
-- declarations within the given translation unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#gaec6e69127920785e74e4a517423f4391>
clang_getTranslationUnitCursor :: CXTranslationUnit -> IO CXCursor
clang_getTranslationUnitCursor unit = wrapForeignPtr =<<
    wrap_malloc_getTranslationUnitCursor unit

-- | Determine whether two cursors are equivalent.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#ga98df58f09878710b983b6f3f60f0cba3>
clang_equalCursors :: CXCursor -> CXCursor -> IO Bool
clang_equalCursors a b =
    unwrapForeignPtr a $ \a' ->
    unwrapForeignPtr b $ \b' ->
      cToBool <$> nowrapper_equalCursors a' b'

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
    unwrapForeignPtr cursor $ \cursor' -> wrapForeignPtr =<<
      wrap_malloc_getCursorSemanticParent cursor'

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
    unwrapForeignPtr cursor $ \cursor' -> wrapForeignPtr =<<
      wrap_malloc_getCursorLexicalParent cursor'

-- | Retrieve the kind of the given cursor.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#ga018aaf60362cb751e517d9f8620d490c>
clang_getCursorKind :: CXCursor -> IO (SimpleEnum CXCursorKind)
clang_getCursorKind cursor =
    unwrapForeignPtr cursor $ \cursor' ->
      wrap_getCursorKind cursor'

-- | Get spelling of a cursor
--
-- NOTE: This is from the @libclang@ \"Debugging facilities\"
-- (https://clang.llvm.org/doxygen/group__CINDEX__DEBUG.html). This should be
-- used only for testing and debugging, and should not be relied upon.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DEBUG.html#ga7a4eecfc1b343568cb9ea447cbde08a8
clang_getCursorKindSpelling :: SimpleEnum CXCursorKind -> IO ByteString
clang_getCursorKindSpelling kind = packCXString =<<
  wrap_malloc_getCursorKindSpelling kind

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
     CXCursor_
     -- ^ The cursor being visited.
  -> CXCursor_
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
  wrap_malloc_visitChildren :: CXCursor_ -> FunPtr CXCursorVisitor -> IO CUInt

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
      current' <- wrapForeignPtr current
      parent'  <- wrapForeignPtr parent
      visitor current' parent'
    unwrapForeignPtr root $ \parent' ->
      -- The @malloc@ does not happen in 'wrap_malloc_visitChildren' itself,
      -- but for each invocation of the visitor (see above).
      (/= 0) <$> wrap_malloc_visitChildren parent' visitor'

{-------------------------------------------------------------------------------
  Cross-referencing in the AST

  <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html>
-------------------------------------------------------------------------------}

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getCursorDisplayName"
  wrap_malloc_getCursorDisplayName :: CXCursor_ -> IO CXString

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getCursorSpelling"
  wrap_malloc_getCursorSpelling :: CXCursor_ -> IO CXString

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getCursorReferenced"
  wrap_malloc_getCursorReferenced :: CXCursor_ -> IO CXCursor_

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getCursorDefinition"
  wrap_malloc_getCursorDefinition :: CXCursor_ -> IO CXCursor_

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getCanonicalCursor"
  wrap_malloc_getCanonicalCursor :: CXCursor_ -> IO CXCursor_

foreign import capi unsafe "clang_wrappers.h wrap_malloc_Cursor_getRawCommentText"
  wrap_malloc_Cursor_getRawCommentText :: CXCursor_ -> IO CXString

foreign import capi unsafe "clang_wrappers.h wrap_malloc_Cursor_getBriefCommentText"
  wrap_malloc_Cursor_getBriefCommentText :: CXCursor_ -> IO CXString

foreign import capi unsafe "clang_wrappers.h wrap_malloc_Cursor_getSpellingNameRange"
  wrap_malloc_Cursor_getSpellingNameRange ::
       CXCursor_
    -> CUInt
    -> CUInt
    -> IO CXSourceRange_

foreign import capi unsafe "clang_wrappers.h wrap_isCursorDefinition"
  nowrapper_isCursorDefinition :: CXCursor_ -> IO CUInt

-- | Retrieve the display name for the entity referenced by this cursor.
--
-- The display name contains extra information that helps identify the cursor,
-- such as the parameters of a function or template or the arguments of a class
-- template specialization.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#gac3eba3224d109a956f9ef96fd4fe5c83>
clang_getCursorDisplayName :: CXCursor -> IO ByteString
clang_getCursorDisplayName cursor =
    unwrapForeignPtr cursor $ \cursor' -> packCXString =<<
      wrap_malloc_getCursorDisplayName cursor'

-- | Retrieve a name for the entity referenced by this cursor.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#gaad1c9b2a1c5ef96cebdbc62f1671c763>
clang_getCursorSpelling :: CXCursor -> IO ByteString
clang_getCursorSpelling cursor =
    unwrapForeignPtr cursor $ \cursor' -> packCXString =<<
      wrap_malloc_getCursorSpelling cursor'

-- | For a cursor that is a reference, retrieve a cursor representing the entity
-- that it references.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#gabf059155921552e19fc2abed5b4ff73a>
clang_getCursorReferenced :: CXCursor -> IO CXCursor
clang_getCursorReferenced cursor =
    unwrapForeignPtr cursor $ \cursor' -> wrapForeignPtr =<<
      wrap_malloc_getCursorReferenced cursor'

-- | For a cursor that is either a reference to or a declaration of some entity,
-- retrieve a cursor that describes the definition of that entity.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#gafcfbec461e561bf13f1e8540bbbd655b>
clang_getCursorDefinition :: CXCursor -> IO CXCursor
clang_getCursorDefinition cursor =
    unwrapForeignPtr cursor $ \cursor' -> wrapForeignPtr =<<
      wrap_malloc_getCursorDefinition cursor'

-- |  Retrieve the canonical cursor corresponding to the given cursor.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#gac802826668be9fd40a017523cc7d24fe>
clang_getCanonicalCursor :: CXCursor -> IO CXCursor
clang_getCanonicalCursor cursor =
    unwrapForeignPtr cursor $ \cursor' -> wrapForeignPtr =<<
      wrap_malloc_getCanonicalCursor cursor'

-- | Given a cursor that represents a declaration, return the associated comment
-- text, including comment markers.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#ga32905a8b1858e67cf5d28b7ad7150779>
clang_Cursor_getRawCommentText :: CXCursor -> IO ByteString
clang_Cursor_getRawCommentText cursor =
    unwrapForeignPtr cursor $ \cursor' -> packCXString =<<
      wrap_malloc_Cursor_getRawCommentText cursor'

-- | Given a cursor that represents a documentable entity (e.g., declaration),
-- return the associated brief comment.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#ga6b5282b915d457d728434c0651ea0b8b>
clang_Cursor_getBriefCommentText :: CXCursor -> IO ByteString
clang_Cursor_getBriefCommentText cursor =
    unwrapForeignPtr cursor $ \cursor' -> packCXString =<<
      wrap_malloc_Cursor_getBriefCommentText cursor'

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
    unwrapForeignPtr cursor $ \cursor' -> wrapForeignPtr =<<
      wrap_malloc_Cursor_getSpellingNameRange cursor' pieceIndex options

-- | Determine whether the declaration pointed to by this cursor is also a
-- definition of that entity.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__XREF.html#ga6ad05634a73e693217088eaa693f0010>
clang_isCursorDefinition :: CXCursor -> IO Bool
clang_isCursorDefinition cursor =
    unwrapForeignPtr cursor $ \cursor' ->
      cToBool <$> nowrapper_isCursorDefinition cursor'

{-------------------------------------------------------------------------------
  Type information for CXCursors

  <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html>
-------------------------------------------------------------------------------}

newtype CXType_ = CXType_ (Ptr ())
  deriving stock (Show)
  deriving newtype (IsPointer)

-- | The type of an element in the abstract syntax tree.
--
-- <https://clang.llvm.org/doxygen/structCXType.html>
newtype CXType = CXType (ForeignPtr ())
  deriving IsForeignPtr via DeriveIsForeignPtr CXType_ CXType

foreign import capi unsafe "clang_wrappers.h wrap_cxtKind"
  wrap_cxtKind :: CXType_ -> IO (SimpleEnum CXTypeKind)

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getCursorType"
  wrap_malloc_getCursorType :: CXCursor_ -> IO CXType_

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getTypeKindSpelling"
  wrap_malloc_getTypeKindSpelling :: SimpleEnum CXTypeKind -> IO CXString

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getTypeSpelling"
  wrap_malloc_getTypeSpelling :: CXType_ -> IO CXString

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getPointeeType"
  wrap_malloc_getPointeeType :: CXType_ -> IO CXType_

foreign import capi unsafe "clang_wrappers.h wrap_Type_getSizeOf"
  wrap_Type_getSizeOf :: CXType_ -> IO CLLong

foreign import capi unsafe "clang_wrappers.h wrap_Type_getAlignOf"
  wrap_getAlignOf :: CXType_ -> IO CLLong

foreign import capi unsafe "clang_wrappers.h wrap_Type_isTransparentTagTypedef"
  wrap_Type_isTransparentTagTypedef :: CXType_ -> IO CUInt

foreign import capi unsafe "clang_wrappers.h wrap_Cursor_isAnonymous"
  wrap_Cursor_isAnonymous :: CXCursor_ -> IO CUInt

foreign import capi unsafe "clang_wrappers.h wrap_getEnumConstantDeclValue"
  wrap_getEnumConstantDeclValue :: CXCursor_ -> IO CLLong

-- | Extract the @kind@ field from a @CXType@ struct
--
-- <https://clang.llvm.org/doxygen/structCXType.html#ab27a7510dc88b0ec80cff04ec89901aa>
cxtKind :: CXType -> SimpleEnum CXTypeKind
cxtKind typ = unsafePerformIO $
    unwrapForeignPtr typ $ \typ' ->
      wrap_cxtKind typ'

-- | Retrieve the type of a CXCursor (if any).
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gaae5702661bb1f2f93038051737de20f4>
clang_getCursorType :: CXCursor -> IO CXType
clang_getCursorType cursor =
    unwrapForeignPtr cursor $ \cursor' -> wrapForeignPtr =<<
      wrap_malloc_getCursorType cursor'

-- | Retrieve the spelling of a given CXTypeKind.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga6bd7b366d998fc67f4178236398d0666>
clang_getTypeKindSpelling :: SimpleEnum CXTypeKind -> IO ByteString
clang_getTypeKindSpelling kind = packCXString =<<
    wrap_malloc_getTypeKindSpelling kind

-- | Pretty-print the underlying type using the rules of the language of the
-- translation unit from which it came.
--
-- Throws 'CallFailed' if the type is invalid.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gac9d37f61bede521d4f42a6553bcbc09f>
clang_getTypeSpelling :: HasCallStack => CXType -> IO ByteString
clang_getTypeSpelling typ = ensure (not . BS.null) $
     unwrapForeignPtr typ $ \typ' -> packCXString =<<
       wrap_malloc_getTypeSpelling typ'

-- | For pointer types, returns the type of the pointee.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gaafa3eb34932d8da1358d50ed949ff3ee>
clang_getPointeeType :: CXType -> IO CXType
clang_getPointeeType typ =
    unwrapForeignPtr typ $ \typ' -> wrapForeignPtr =<<
      wrap_malloc_getPointeeType typ'

-- | Return the size of a type in bytes as per @C++[expr.sizeof]@ standard.
--
-- Throws 'CallFailed' with 'CXTypeLayoutError' on error.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga027abe334546e80931905f31399d0a8b>
clang_Type_getSizeOf :: HasCallStack => CXType -> IO CLLong
clang_Type_getSizeOf typ =
    unwrapForeignPtr typ $ \typ' -> ensureNotInRange @CXTypeLayoutError $
      wrap_Type_getSizeOf typ'

-- | Return the alignment of a type in bytes as per C++[expr.alignof] standard.
--
-- Throws 'CallFailed' with 'CXTypeLayoutError' on error.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#gaee56de66c69ab5605fe47e7c52497e31>
clang_Type_getAlignOf :: HasCallStack => CXType -> IO CLLong
clang_Type_getAlignOf typ =
    unwrapForeignPtr typ $ \typ' -> ensureNotInRange @CXTypeLayoutError $
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
    unwrapForeignPtr typ $ \typ' ->
      cToBool <$> wrap_Type_isTransparentTagTypedef typ'

-- | Determine whether the given cursor represents an anonymous tag or
-- namespace.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__TYPES.html#ga6e0d2674d126fd43816ce3a80b592373>
clang_Cursor_isAnonymous :: CXCursor -> IO Bool
clang_Cursor_isAnonymous cursor =
    unwrapForeignPtr cursor $ \cursor' ->
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

    unwrapForeignPtr cursor $ \cursor' ->
      wrap_getEnumConstantDeclValue cursor'

{-------------------------------------------------------------------------------
  Mapping between cursors and source code

  <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__SOURCE.html>
-------------------------------------------------------------------------------}

newtype CXSourceRange_ = CXSourceRange_ (Ptr ())
  deriving stock (Show)
  deriving newtype (IsPointer)

-- | Identifies a half-open character range in the source code.
--
-- Use 'clang_getRangeStart' and 'clang_getRangeEnd' to retrieve the starting
-- and end locations from a source range, respectively.
--
-- <https://clang.llvm.org/doxygen/structCXSourceRange.html>
newtype CXSourceRange = CXSourceRange (ForeignPtr ())
  deriving IsForeignPtr via DeriveIsForeignPtr CXSourceRange_ CXSourceRange

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getCursorLocation"
  wrap_malloc_getCursorLocation :: CXCursor_ -> IO CXSourceLocation_

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getCursorExtent"
  wrap_malloc_getCursorExtent :: CXCursor_ -> IO CXSourceRange_

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
    unwrapForeignPtr cursor $ \cursor' -> wrapForeignPtr =<<
      wrap_malloc_getCursorLocation cursor'

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
    unwrapForeignPtr cursor $ \cursor' -> wrapForeignPtr =<<
      wrap_malloc_getCursorExtent cursor'

{-------------------------------------------------------------------------------
  Token extraction and manipulation

  <https://clang.llvm.org/doxygen/group__CINDEX__LEX.html>
-------------------------------------------------------------------------------}

newtype CXToken = CXToken (Ptr ())
  deriving stock (Show)
  deriving newtype (IsPointer)

foreign import capi unsafe "clang_wrappers.h wrap_getToken"
  wrap_getToken :: CXTranslationUnit -> CXSourceLocation_ -> IO CXToken

foreign import capi unsafe "clang_wrappers.h wrap_getTokenKind"
  clang_getTokenKind :: CXToken -> IO (SimpleEnum CXTokenKind)

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getTokenSpelling"
  wrap_malloc_getTokenSpelling :: CXTranslationUnit -> CXToken -> IO CXString

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getTokenLocation"
  wrap_malloc_getTokenLocation ::
       CXTranslationUnit
    -> CXToken
    -> IO CXSourceLocation_

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getTokenExtent"
  wrap_malloc_getTokenExtent ::
       CXTranslationUnit
    -> CXToken
    -> IO CXSourceRange_

-- | Get the raw lexical token starting with the given location.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LEX.html#ga3b41b2c8a34e605a14608927ae544c03>
clang_getToken :: CXTranslationUnit -> CXSourceLocation -> IO (Maybe CXToken)
clang_getToken unit loc = checkNotNull $
    unwrapForeignPtr loc $ \loc' ->
      wrap_getToken unit loc'

-- | Determine the spelling of the given token.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LEX.html#ga1033a25c9d2c59bcbdb23020de0bba2c>
clang_getTokenSpelling :: CXTranslationUnit -> CXToken -> IO ByteString
clang_getTokenSpelling unit token = packCXString =<<
    wrap_malloc_getTokenSpelling unit token

-- | Retrieve the source location of the given token.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LEX.html#ga76a721514acb4cc523e10a6913d88021>
clang_getTokenLocation :: CXTranslationUnit -> CXToken -> IO CXSourceLocation
clang_getTokenLocation unit token = wrapForeignPtr =<<
    wrap_malloc_getTokenLocation unit token

-- | Retrieve a source range that covers the given token.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LEX.html#ga5acbc0a2a3c01aa44e1c5c5ccc4e328b>
clang_getTokenExtent :: CXTranslationUnit -> CXToken -> IO CXSourceRange
clang_getTokenExtent unit token = wrapForeignPtr =<<
    wrap_malloc_getTokenExtent unit token

{-------------------------------------------------------------------------------
  Physical source locations

  <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html>
-------------------------------------------------------------------------------}

newtype CXSourceLocation_ = CXSourceLocation_ (Ptr ())
  deriving stock (Show)
  deriving newtype (IsPointer)

-- | Identifies a specific source location within a translation unit.
--
-- Use 'clang_getExpansionLocation' or 'clang_getSpellingLocation' to map a
-- source location to a particular file, line, and column.
--
-- <https://clang.llvm.org/doxygen/structCXSourceLocation.html>
newtype CXSourceLocation = CXSourceLocation (ForeignPtr ())
  deriving IsForeignPtr via DeriveIsForeignPtr CXSourceLocation_ CXSourceLocation

-- | A particular source file that is part of a translation unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__FILES.html#gacfcea9c1239c916597e2e5b3e109215a>
newtype CXFile = CXFile (Ptr ())
  deriving stock (Show)
  deriving newtype (IsPointer, Storable)

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getRangeStart"
  wrap_malloc_getRangeStart :: CXSourceRange_ -> IO CXSourceLocation_

foreign import capi unsafe "clang_wrappers.h wrap_malloc_getRangeEnd"
  wrap_malloc_getRangeEnd :: CXSourceRange_ -> IO CXSourceLocation_

foreign import capi "clang_wrappers.h wrap_getExpansionLocation"
  wrap_getExpansionLocation ::
       CXSourceLocation_
       -- ^ the location within a source file that will be decomposed into its parts.
    -> Ptr CXFile
       -- ^ [out] if non-NULL, will be set to the file to which the given source location points.
    -> Ptr CUInt
       -- ^ [out] if non-NULL, will be set to the line to which the given source location points.
    -> Ptr CUInt
       -- ^ [out] if non-NULL, will be set to the column to which the given source location points.
    -> Ptr CUInt
       -- ^ [out] if non-NULL, will be set to the offset into the buffer to which the given source location points.
    -> IO ()

foreign import capi "clang_wrappers.h wrap_getSpellingLocation"
  wrap_getSpellingLocation ::
       CXSourceLocation_
       -- ^ the location within a source file that will be decomposed into its parts.
    -> Ptr CXFile
       -- ^ [out] if non-NULL, will be set to the file to which the given source location points.
    -> Ptr CUInt
       -- ^ [out] if non-NULL, will be set to the line to which the given source location points.
    -> Ptr CUInt
       -- ^ [out] if non-NULL, will be set to the column to which the given source location points.
    -> Ptr CUInt
       -- ^ [out] if non-NULL, will be set to the offset into the buffer to which the given source location points.
    -> IO ()

foreign import capi "clang_wrappers.h wrap_Location_isFromMainFile"
  wrap_Location_isFromMainFile :: CXSourceLocation_ -> IO CInt

-- | Retrieve a source location representing the first character within a source
-- range.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#gac2cc034e3965739c41662f6ada7ff248>
clang_getRangeStart :: CXSourceRange -> IO CXSourceLocation
clang_getRangeStart range =
    unwrapForeignPtr range $ \range' -> wrapForeignPtr =<<
      wrap_malloc_getRangeStart range'

-- | Retrieve a source location representing the last character within a source
-- range.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#gacdb7d3c2b77a06bcc2e83bde3e14c3c0>
clang_getRangeEnd :: CXSourceRange -> IO CXSourceLocation
clang_getRangeEnd range =
    unwrapForeignPtr range $ \range' -> wrapForeignPtr =<<
      wrap_malloc_getRangeEnd range'

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
     CXSourceLocation
  -> IO (CXFile, CUInt, CUInt, CUInt)
clang_getExpansionLocation location =
     unwrapForeignPtr location $ \location' ->
       alloca $ \file ->
       alloca $ \line ->
       alloca $ \column ->
       alloca $ \offset -> do
         wrap_getExpansionLocation location' file line column offset
         (,,,) <$> peek file <*> peek line <*> peek column <*> peek offset

-- | Retrieve the file, line, column, and offset represented by the given source
-- location.
--
-- If the location refers into a macro instantiation, return where the location
-- was originally spelled in the source file.
--
-- See also 'clang_getExpansionLocation'.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#ga01f1a342f7807ea742aedd2c61c46fa0>
clang_getSpellingLocation ::
     CXSourceLocation
  -> IO (CXFile, CUInt, CUInt, CUInt)
clang_getSpellingLocation location =
     unwrapForeignPtr location $ \location' ->
       alloca $ \file ->
       alloca $ \line ->
       alloca $ \column ->
       alloca $ \offset -> do
         wrap_getSpellingLocation location' file line column offset
         (,,,) <$> peek file <*> peek line <*> peek column <*> peek offset

-- | Check if the given source location is in the main file of the corresponding
-- translation unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#gacb4ca7b858d66f0205797ae84cc4e8f2>
clang_Location_isFromMainFile :: CXSourceLocation -> IO Bool
clang_Location_isFromMainFile location =
    unwrapForeignPtr location $ \location' ->
      cToBool <$> wrap_Location_isFromMainFile location'

{-------------------------------------------------------------------------------
  File manipulation routines

  <https://clang.llvm.org/doxygen/group__CINDEX__FILES.html>
-------------------------------------------------------------------------------}

foreign import capi "clang_wrappers.h wrap_malloc_getFileName"
  wrap_malloc_getFileName :: CXFile -> IO CXString

-- | Retrieve the complete file and path name of the given file.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__FILES.html#ga626ff6335ab1e0a2b8c8823301225690>
clang_getFileName :: CXFile -> IO ByteString
clang_getFileName file = packCXString =<< wrap_malloc_getFileName file
