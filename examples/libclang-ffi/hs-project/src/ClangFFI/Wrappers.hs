-- MagicHash / UnboxedTuples: 'withFrozenW' freezes an unlifted W buffer in place.
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

-- | High-level wrappers over libclang's raw @Clang.LowLevel.FFI@, written with the
-- @HsBindgen.HighLevel@ combinators plus the by-value plumbing the raw imports need
-- (@Clang.Internal.ByValue@, @Clang.Enum.Simple@).
--
-- This is the AST-walk slice: index and translation-unit lifecycle, cursor queries,
-- source locations, and @clang_visitChildren@.
--
-- libclang passes cursors, locations and strings by value, which GHC's FFI cannot
-- do, so they arrive as /unlifted/ byte arrays: @R@ going in, @W@ coming out.
-- 'bracketUnlifted' supplies an @R@ argument and 'outputUnlifted' fills a @W@
-- out-parameter; otherwise the specs here read like any other.
--
-- @clang_visitChildren@ is the exception, and only because it takes a callback: it
-- goes through the C trampoline in @cbits/hs_visit.c@.
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

import Control.Exception (Exception, SomeException, finally, throwIO, try)
import Control.Monad (void, when)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Foreign.C.Types (CInt (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import GHC.Exts (unsafeFreezeByteArray#)
import GHC.IO (IO (..))
import GHC.Ptr (Ptr (..))

import Clang.Enum.Simple (SimpleEnum (..), simpleEnum)
import Clang.Internal.ByValue (HasKnownSize, OnHaskellHeap (..), R (..), W (..),
                               copyToHaskellHeap, onHaskellHeap, preallocate)
import Clang.Internal.ConstPtr (ConstPtr (..))
import Clang.LowLevel.Core.Enums (CXChildVisitResult (..), CXCursorKind)
import Clang.LowLevel.Core.Instances ()
import Clang.LowLevel.Core.Pointers (CXIndex, CXTranslationUnit)
import Clang.LowLevel.Core.Structs (CXCursor_, CXSourceLocation_, CXString_)
import Clang.LowLevel.FFI

import HsBindgen.Runtime.Support.FunPtr (withFunPtrAs)

import HsBindgen.HighLevel (fixed, input, output, scratch, toHighLevel)
import HsBindgen.HighLevel.Auto (auto, autoInputs, autoResult, checkedResult)
import HsBindgen.HighLevel.Defaults (defaultIn, defaultOut)
import HsBindgen.HighLevel.Marshaller (Unmarshaller (..), unmarshalOutPure)
import HsBindgen.HighLevel.Unlifted (bracketUnlifted, outputUnlifted)

{-------------------------------------------------------------------------------
  Index and translation-unit lifecycle
-------------------------------------------------------------------------------}

-- | @clang_createIndex@. Fully lifted, so @auto@ fills both inputs and the result.
createIndex :: Int -> Int -> IO CXIndex
createIndex = toHighLevel nowrapper_createIndex auto

-- | @clang_disposeIndex@.
disposeIndex :: CXIndex -> IO ()
disposeIndex = toHighLevel nowrapper_disposeIndex auto

-- | @clang_disposeTranslationUnit@.
disposeTranslationUnit :: CXTranslationUnit -> IO ()
disposeTranslationUnit = toHighLevel nowrapper_disposeTranslationUnit auto

-- | @clang_parseTranslationUnit2@. Unsaved files and parse options are not exposed;
-- this wrapper pins them, and always parses from a file on disk.
--
-- Failure is reported in the return value while the translation unit arrives through
-- a @CXTranslationUnit *@, so a non-zero @CXErrorCode@ throws 'ParseFailed' and the
-- out-parameter is never read.
parseTU :: CXIndex -> FilePath -> [String] -> IO CXTranslationUnit
parseTU idx path args =
  toHighLevel nowrapper_parseTranslationUnit2
              ( autoInputs                         -- CXIndex, source, argv
              $ fixed (fromIntegral (length args)) -- int num_args
              $ fixed nullPtr                      -- CXUnsavedFile *unsaved
              $ fixed 0                            -- unsigned num_unsaved
              $ fixed 0                            -- unsigned options
              $ output (unmarshalOutPure id)       -- CXTranslationUnit *out
              $ checkedResult checkParsed
              ) idx path args
  where
    -- Zero is success. Written out rather than reached for with 'throwOnNonZero'
    -- because the status is a @SimpleEnum@, not a 'Num'.
    checkParsed (SimpleEnum c) =
      when (c /= 0) $ throwIO (ParseFailed ("CXErrorCode " ++ show c))

-- | Thrown when @clang_parseTranslationUnit2@ reports a nonzero @CXErrorCode@.
newtype ParseFailed = ParseFailed String
  deriving stock (Show)

instance Exception ParseFailed

{-------------------------------------------------------------------------------
  Cursor queries

  Each takes a cursor or location by value (an unlifted @R@) and \/ or fills a
  by-value out-parameter (an unlifted @W@). 'bracketUnlifted' 'onHaskellHeap'
  supplies the @R@ argument and 'outputUnlifted' hosts the @W@ out-parameter.

  The two Unmarshallers below are built by hand. An 'Unmarshaller' wants an allocator
  and a reader separately, since the allocation happens before the C call and the
  read-back after it, whereas libclang's 'preallocate' does both in one bracket: it
  allocates a @W@ buffer, runs a continuation, then freezes the result. So they take
  the allocation from 'preallocate' and do the freeze themselves.
-------------------------------------------------------------------------------}

-- | Allocate a @W tag@ write buffer for the duration of the call.
--
-- 'preallocate' also freezes the buffer into an 'OnHaskellHeap' as it returns. That
-- frozen copy is discarded here, since the readers below do their own freezing.
-- Freezing is a cast performed in place, so the discarded one costs nothing.
allocW :: forall tag r. HasKnownSize tag => (W tag -> IO r) -> IO r
allocW = fmap snd . preallocate @(OnHaskellHeap tag)

-- | Freeze a @W@ write buffer in place and hand the continuation the read-only @R@
-- view of the same bytes. Both are unlifted, so the frozen array has to be passed
-- to a continuation rather than returned in 'IO'.
withFrozenW :: W tag -> (R tag -> IO r) -> IO r
withFrozenW (W marr) k = IO $ \s0 ->
    case unsafeFreezeByteArray# marr s0 of
      (# s1, arr #) -> unIO (k (R arr)) s1
  where
    unIO (IO f) = f

-- | A @W@ by-value struct out-parameter read back onto the Haskell heap.
onHeapOut :: HasKnownSize tag => Unmarshaller (W tag) (OnHaskellHeap tag)
onHeapOut = Unmarshaller allocW $ \w ->
    withFrozenW w $ \(R arr) -> pure (OnHaskellHeap arr)

-- | A @W CXString_@ out-parameter read back as 'Text': freeze the buffer, copy the
-- UTF-8 payload out of the @CXString@, and dispose it. The copy has to happen here,
-- inside the reader, because @clang_disposeString@ frees the bytes it points at.
cxStringOut :: Unmarshaller (W CXString_) Text
cxStringOut = Unmarshaller allocW $ \w ->
    withFrozenW w $ \r -> copyOut r `finally` wrap_disposeString r
  where
    copyOut r = do
      ConstPtr p@(Ptr addr) <- wrap_getCString r
      pure $! if p == nullPtr then T.empty else T.unpackCString# addr

-- Each of these returns @void@ and delivers its answer through a by-value
-- out-parameter.

-- | @clang_getTranslationUnitCursor@.
tuCursor :: CXTranslationUnit -> IO (OnHaskellHeap CXCursor_)
tuCursor = toHighLevel wrap_getTranslationUnitCursor
         $ input          defaultIn -- CXTranslationUnit
         $ outputUnlifted onHeapOut -- W CXCursor_ (out)
         $ autoResult

-- | @clang_getCursorKind@. The only cursor query that returns its answer directly.
cursorKind :: OnHaskellHeap CXCursor_ -> IO (SimpleEnum CXCursorKind)
cursorKind = toHighLevel wrap_getCursorKind
           $ input (bracketUnlifted onHaskellHeap) -- R CXCursor_
           $ autoResult

-- | @clang_getCursorKindSpelling@.
kindSpelling :: SimpleEnum CXCursorKind -> IO Text
kindSpelling = toHighLevel wrap_getCursorKindSpelling
             $ input          defaultIn   -- SimpleEnum CXCursorKind
             $ outputUnlifted cxStringOut -- W CXString_ (out)
             $ autoResult

-- | @clang_getCursorSpelling@.
cursorSpelling :: OnHaskellHeap CXCursor_ -> IO Text
cursorSpelling = toHighLevel wrap_getCursorSpelling
               $ input          (bracketUnlifted onHaskellHeap) -- R CXCursor_
               $ outputUnlifted cxStringOut                     -- W CXString_ (out)
               $ autoResult

-- | @clang_getCursorLocation@.
cursorLocation :: OnHaskellHeap CXCursor_ -> IO (OnHaskellHeap CXSourceLocation_)
cursorLocation = toHighLevel wrap_getCursorLocation
               $ input          (bracketUnlifted onHaskellHeap) -- R CXCursor_
               $ outputUnlifted onHeapOut                       -- W CXSourceLocation_ (out)
               $ autoResult

-- | @clang_getSpellingLocation@. Mixes the two representations: the location goes in
-- by value as an unlifted @R@, while the four out-parameters are ordinary lifted
-- @Ptr@s.
--
-- libclang requires somewhere to write the file and byte offset even when the caller
-- wants neither, so those two slots are allocated for the call and dropped.
spellingLineCol :: OnHaskellHeap CXSourceLocation_ -> IO (Word, Word)
spellingLineCol = toHighLevel wrap_getSpellingLocation
                $ input   (bracketUnlifted onHaskellHeap) -- R CXSourceLocation_
                $ scratch alloca     -- CXFile *file     (written, never read)
                $ output  defaultOut -- unsigned *line
                $ output  defaultOut -- unsigned *column
                $ scratch alloca     -- unsigned *offset (written, never read)
                $ autoResult

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
