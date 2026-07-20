-- | High-level wrappers over libclang's raw @Clang.LowLevel.FFI@, written with the
-- @HsBindgen.Runtime.HighLevel@ combinators plus the by-value plumbing the raw
-- imports need (@Clang.Internal.ByValue@, @Clang.Internal.CXString@,
-- @Clang.Enum.Simple@).
--
-- This is the AST-walk slice: index and translation-unit lifecycle, cursor
-- queries, source locations, and @clang_visitChildren@. The combinators handle the
-- lifted positions; the by-value cursor \/ string \/ location arguments are
-- unlifted (@R@ \/ @W@) and supplied by hand with 'onHaskellHeap' \/ 'preallocate_'.
module ClangFFI.Wrappers (
    -- * Index and translation-unit lifecycle
    createIndex
  , disposeIndex
  , parseTU
  , disposeTranslationUnit
  , ParseFailed (..)
    -- * Cursors and traversal
  , tuCursor
  , cursorKind
  , kindSpelling
  , cursorSpelling
  , cursorLocation
  , spellingLineCol
  , visitChildren
  , childrenOf
  ) where

import Control.Exception (Exception, SomeException, throwIO, try)
import Control.Monad (void)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Void (Void)
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar, CInt (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)

import Clang.Enum.Simple (SimpleEnum (..), simpleEnum)
import Clang.Internal.ByValue (OnHaskellHeap, R (..), copyToHaskellHeap,
                               onHaskellHeap, preallocate_)
import Clang.Internal.ConstPtr (ConstPtr (..))
import Clang.Internal.CXString ()
import Clang.LowLevel.Core.Enums (CXChildVisitResult (..), CXCursorKind,
                                  CXErrorCode)
import Clang.LowLevel.Core.Instances ()
import Clang.LowLevel.Core.Pointers (CXIndex, CXTranslationUnit)
import Clang.LowLevel.Core.Structs (CXCursor_, CXSourceLocation_, CXUnsavedFile)
import Clang.LowLevel.FFI

import HsBindgen.Runtime.HighLevel (ToHighLevel, discardResult, fixed, input,
                                    output, resultIO, scratch, toHighLevel)
import HsBindgen.Runtime.HighLevel.Defaults (auto, defaultOut)
import HsBindgen.Runtime.HighLevel.Marshaller (Marshal (..), scalar,
                                               unmarshalOutPure)
import HsBindgen.Runtime.HighLevel.Marshaller.Utils (withCStringIn)

import ClangFFI.Callback (withFunPtrAs)

{-------------------------------------------------------------------------------
  Index and translation-unit lifecycle
-------------------------------------------------------------------------------}

-- | @clang_createIndex@. Fully lifted, so @auto@ fills both inputs and the result.
createIndex :: Int -> Int -> IO CXIndex
createIndex = toHighLevel auto nowrapper_createIndex

-- | @clang_disposeIndex@.
disposeIndex :: CXIndex -> IO ()
disposeIndex = toHighLevel auto nowrapper_disposeIndex

-- | @clang_disposeTranslationUnit@.
disposeTranslationUnit :: CXTranslationUnit -> IO ()
disposeTranslationUnit = toHighLevel auto nowrapper_disposeTranslationUnit

-- | @clang_parseTranslationUnit2@: 'output' for the out-parameter translation
-- unit, 'withCStringIn' for the source path, 'argvIn' for the argument array, and
-- 'fixed' for the arguments we pin. 'statusClose' classifies the result.
parseTU :: CXIndex -> FilePath -> [String] -> IO CXTranslationUnit
parseTU idx path args =
    fst <$> toHighLevel
              ( input (scalar id)                          -- CXIndex
              $ input withCStringIn                        -- const char *source
              $ input argvIn                               -- const char *const *argv
              $ fixed (fromIntegral (length args) :: CInt) -- int num_args
              $ fixed (nullPtr :: Ptr CXUnsavedFile)       -- CXUnsavedFile *unsaved
              $ fixed (0 :: CUInt)                         -- unsigned num_unsaved
              $ fixed (0 :: CUInt)                         -- unsigned options
              $ output (unmarshalOutPure id)               -- CXTranslationUnit *out
              $ statusClose
              )
              nowrapper_parseTranslationUnit2 idx path args
  where
    -- @[String]@ to a @const char *const *@. There is no ready-made marshaller for
    -- a string array, so build one from the 'Marshal' constructor; the length is a
    -- separate 'fixed' at the call site.
    argvIn :: Marshal [String] (ConstPtr (ConstPtr CChar) -> lo') lo'
    argvIn = Marshal $ \as lo k ->
        withMany withCString as $ \cstrs ->
          withArray (map ConstPtr cstrs) $ \arr ->
            k (lo (ConstPtr arr))

    -- Throw on a nonzero @CXErrorCode@ (0 is success). Hand-written because the
    -- status is a @SimpleEnum@, not a 'Num', so 'throwOnNonZero' does not apply.
    statusClose :: ToHighLevel (IO (SimpleEnum (Maybe CXErrorCode))) (IO ())
    statusClose = resultIO $ \(SimpleEnum c) ->
        if c == 0
          then pure ()
          else throwIO (ParseFailed ("CXErrorCode " ++ show c))

-- | Thrown when @clang_parseTranslationUnit2@ reports a nonzero @CXErrorCode@.
newtype ParseFailed = ParseFailed String
  deriving stock (Show)

instance Exception ParseFailed

{-------------------------------------------------------------------------------
  Cursor queries

  Each takes a cursor \/ location by value (@R@) and \/ or fills a by-value
  out-parameter (@W@). Both are unlifted, so no combinator applies: the argument is
  passed with 'onHaskellHeap' and the result read back with 'preallocate_'
  ('preallocate_' at @Text@ uses the @Preallocate Text@ instance, which copies the
  @CXString@ out and disposes it).
-------------------------------------------------------------------------------}

-- | @clang_getTranslationUnitCursor@.
tuCursor :: CXTranslationUnit -> IO (OnHaskellHeap CXCursor_)
tuCursor tu = preallocate_ (wrap_getTranslationUnitCursor tu)

-- | @clang_getCursorKind@.
cursorKind :: OnHaskellHeap CXCursor_ -> IO (SimpleEnum CXCursorKind)
cursorKind cur = onHaskellHeap cur wrap_getCursorKind

-- | @clang_getCursorKindSpelling@.
kindSpelling :: SimpleEnum CXCursorKind -> IO Text
kindSpelling k = preallocate_ @Text (wrap_getCursorKindSpelling k)

-- | @clang_getCursorSpelling@.
cursorSpelling :: OnHaskellHeap CXCursor_ -> IO Text
cursorSpelling cur =
    onHaskellHeap cur $ \r -> preallocate_ @Text (wrap_getCursorSpelling r)

-- | @clang_getCursorLocation@.
cursorLocation :: OnHaskellHeap CXCursor_ -> IO (OnHaskellHeap CXSourceLocation_)
cursorLocation cur =
    onHaskellHeap cur $ \r -> preallocate_ (wrap_getCursorLocation r)

-- | @clang_getSpellingLocation@. Mixed: the location is by-value (by hand), but
-- the four out-parameters are lifted @Ptr@s, so the combinators fill them:
-- 'scratch' the file and offset we ignore, 'output' the line and column.
spellingLineCol :: OnHaskellHeap CXSourceLocation_ -> IO (Word, Word)
spellingLineCol loc =
    onHaskellHeap loc $ \r ->
      dropUnit
        <$> toHighLevel
              ( scratch alloca    -- CXFile *file   (ignored)
              $ output defaultOut -- unsigned *line
              $ output defaultOut -- unsigned *column
              $ scratch alloca    -- unsigned *offset (ignored)
              $ discardResult
              )
              (wrap_getSpellingLocation r)
  where
    dropUnit :: (a, b, ()) -> (a, b)
    dropUnit (a, b, ()) = (a, b)

{-------------------------------------------------------------------------------
  Traversal

  clang_visitChildren passes cursors by value, which the Haskell FFI cannot do, so
  it is omitted from the raw FFI. We go through a C trampoline (cbits/hs_visit.c)
  that takes the parent by pointer and calls a pointer-taking Haskell visitor
  (smuggled through client_data).
-------------------------------------------------------------------------------}

-- | The visitor in its natural types: two cursor pointers, a @CXChildVisitResult@.
type Visitor = Ptr CXCursor_ -> Ptr CXCursor_ -> IO (SimpleEnum CXChildVisitResult)

-- | The @ToFunPtr@-covered signature 'Visitor' coerces to: a @Ptr a@ is phantom in
-- @a@ and a @SimpleEnum e@ is a newtype over @CInt@, so the two are 'Coercible'.
type CoveredVisitor = Ptr Void -> Ptr Void -> IO CInt

foreign import capi safe "hs_visit.h hs_visitChildren"
  c_hs_visitChildren :: R CXCursor_ -> FunPtr CoveredVisitor -> IO CUInt

-- | @clang_visitChildren@. 'withFunPtrAs' retags the domain-typed 'Visitor' to
-- 'CoveredVisitor' and reuses the runtime's generated 'ToFunPtr', so no
-- @foreign import "wrapper"@ is needed. The callback copies each stack cursor to
-- the heap; a Haskell exception is stashed and re-thrown after the call, since it
-- cannot cross the C frame.
visitChildren ::
     OnHaskellHeap CXCursor_
  -> (OnHaskellHeap CXCursor_ -> IO CXChildVisitResult)
  -> IO ()
visitChildren parent visit = do
    excRef <- newIORef Nothing
    let wrapped :: Visitor
        wrapped cursorPtr _parentPtr = do
          r <- try (copyToHaskellHeap cursorPtr >>= visit)
          case r of
            Right res -> pure (simpleEnum res)
            Left e -> do
              writeIORef excRef (Just (e :: SomeException))
              pure (simpleEnum CXChildVisit_Break)
    withFunPtrAs @CoveredVisitor wrapped $ \fp ->
      onHaskellHeap parent $ \r -> void (c_hs_visitChildren r fp)
    readIORef excRef >>= mapM_ throwIO

-- | Collect a cursor's direct children (each copied to the heap).
childrenOf :: OnHaskellHeap CXCursor_ -> IO [OnHaskellHeap CXCursor_]
childrenOf cur = do
    ref <- newIORef []
    visitChildren cur $ \child -> do
      modifyIORef' ref (child :)
      pure CXChildVisit_Continue
    reverse <$> readIORef ref
