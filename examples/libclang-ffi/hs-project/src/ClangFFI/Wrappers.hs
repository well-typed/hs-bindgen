-- | High-level wrappers over libclang's raw @Clang.LowLevel.FFI@, written with the
-- @HsBindgen.HighLevel@ combinators plus the by-value plumbing the raw
-- imports need (@Clang.Internal.ByValue@, @Clang.Internal.CXString@,
-- @Clang.Enum.Simple@).
--
-- This is the AST-walk slice: index and translation-unit lifecycle, cursor
-- queries, source locations, and @clang_visitChildren@. The combinators handle both
-- the lifted positions and, through the levity-polymorphic 'bracketUnlifted' \/
-- 'outputUnlifted', the by-value cursor \/ string \/ location arguments passed as
-- unlifted @R@ \/ @W@ (only @clang_visitChildren@ still needs a C trampoline).
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
import Foreign.C.Types (CInt (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)

import Clang.Enum.Simple (SimpleEnum (..), simpleEnum)
import Clang.Internal.ByValue (HasKnownSize, OnHaskellHeap, R (..), W,
                               copyToHaskellHeap, onHaskellHeap, preallocate)
import Clang.Internal.CXString ()
import Clang.LowLevel.Core.Enums (CXChildVisitResult (..), CXCursorKind,
                                  CXErrorCode)
import Clang.LowLevel.Core.Instances ()
import Clang.LowLevel.Core.Pointers (CXIndex, CXTranslationUnit)
import Clang.LowLevel.Core.Structs (CXCursor_, CXSourceLocation_, CXString_,
                                    CXUnsavedFile)
import Clang.LowLevel.FFI

import HsBindgen.Runtime.Internal.FunPtr (withFunPtrAs)

import HsBindgen.HighLevel (ToHighLevel, discardResult, dropTrailingUnit, fixed,
                            input, output, resultIO, resultPure, scratch,
                            toHighLevel)
import HsBindgen.HighLevel.Defaults (auto, defaultOut)
import HsBindgen.HighLevel.Marshaller (Unmarshaller (..), scalar,
                                       unmarshalOutPure)
import HsBindgen.HighLevel.Marshaller.Utils (withCStringArrayIn, withCStringIn)
import HsBindgen.HighLevel.Unlifted (bracketUnlifted, outputUnlifted)

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
-- unit, 'withCStringIn' for the source path, 'withCStringArrayIn' for the argument
-- array, and 'fixed' for the arguments we pin. 'statusClose' classifies the result,
-- and 'dropTrailingUnit' removes the @()@ it leaves beside the out-parameter.
parseTU :: CXIndex -> FilePath -> [String] -> IO CXTranslationUnit
parseTU idx path args =
    toHighLevel
      ( dropTrailingUnit
      $ input (scalar id)                          -- CXIndex
      $ input withCStringIn                        -- const char *source
      $ input withCStringArrayIn                   -- const char *const *argv
      $ fixed (fromIntegral (length args) :: CInt) -- int num_args
      $ fixed (nullPtr :: Ptr CXUnsavedFile)       -- CXUnsavedFile *unsaved
      $ fixed (0 :: CUInt)                         -- unsigned num_unsaved
      $ fixed (0 :: CUInt)                         -- unsigned options
      $ output (unmarshalOutPure id)               -- CXTranslationUnit *out
      $ statusClose
      )
      nowrapper_parseTranslationUnit2 idx path args
  where
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
  out-parameter (@W@). Both are unlifted, but the levity-polymorphic combinators
  reach them: 'bracketUnlifted' 'onHaskellHeap' supplies an @R@ argument through
  'input', and 'outputUnlifted' hosts a @W@ out-parameter via an 'Unmarshaller' over
  'preallocate'. 'cxStringOut' is libclang's CXString copy-and-dispose (the
  @Preallocate Text@ instance), the out-marshaller that could not exist before the
  vocabulary became levity-polymorphic.
-------------------------------------------------------------------------------}

-- | A @W CXString@ out-parameter read back as 'Text' (copied out and disposed by
-- the @Preallocate Text@ instance).
cxStringOut :: Unmarshaller (W CXString_) Text
cxStringOut = Unmarshaller (preallocate @Text)

-- | A @W@ by-value struct out-parameter read back onto the Haskell heap.
onHeapOut :: HasKnownSize tag => Unmarshaller (W tag) (OnHaskellHeap tag)
onHeapOut = Unmarshaller preallocate

-- | @clang_getTranslationUnitCursor@.
tuCursor :: CXTranslationUnit -> IO (OnHaskellHeap CXCursor_)
tuCursor = toHighLevel
    ( dropTrailingUnit
    $ input          (scalar id)   -- CXTranslationUnit
    $ outputUnlifted onHeapOut     -- W CXCursor_ (out)
    $ discardResult
    ) wrap_getTranslationUnitCursor

-- | @clang_getCursorKind@.
cursorKind :: OnHaskellHeap CXCursor_ -> IO (SimpleEnum CXCursorKind)
cursorKind = toHighLevel
    ( input (bracketUnlifted onHaskellHeap)  -- R CXCursor_
    $ resultPure id
    ) wrap_getCursorKind

-- | @clang_getCursorKindSpelling@.
kindSpelling :: SimpleEnum CXCursorKind -> IO Text
kindSpelling = toHighLevel
    ( dropTrailingUnit
    $ input          (scalar id)   -- SimpleEnum CXCursorKind
    $ outputUnlifted cxStringOut   -- W CXString_ (out)
    $ discardResult
    ) wrap_getCursorKindSpelling

-- | @clang_getCursorSpelling@.
cursorSpelling :: OnHaskellHeap CXCursor_ -> IO Text
cursorSpelling = toHighLevel
    ( dropTrailingUnit
    $ input          (bracketUnlifted onHaskellHeap)  -- R CXCursor_
    $ outputUnlifted cxStringOut                        -- W CXString_ (out)
    $ discardResult
    ) wrap_getCursorSpelling

-- | @clang_getCursorLocation@.
cursorLocation :: OnHaskellHeap CXCursor_ -> IO (OnHaskellHeap CXSourceLocation_)
cursorLocation = toHighLevel
    ( dropTrailingUnit
    $ input          (bracketUnlifted onHaskellHeap)  -- R CXCursor_
    $ outputUnlifted onHeapOut                          -- W CXSourceLocation_ (out)
    $ discardResult
    ) wrap_getCursorLocation

-- | @clang_getSpellingLocation@. Mixed: the location is by-value ('bracketUnlifted'
-- 'onHaskellHeap' fills the @R@ argument), and the four out-parameters are lifted
-- @Ptr@s, so 'scratch' hides the file and offset and 'output' keeps the line and
-- column.
spellingLineCol :: OnHaskellHeap CXSourceLocation_ -> IO (Word, Word)
spellingLineCol = toHighLevel
    ( dropTrailingUnit
    $ input   (bracketUnlifted onHaskellHeap)  -- R CXSourceLocation_
    $ scratch alloca     -- CXFile *file   (ignored)
    $ output  defaultOut -- unsigned *line
    $ output  defaultOut -- unsigned *column
    $ scratch alloca     -- unsigned *offset (ignored)
    $ discardResult
    ) wrap_getSpellingLocation

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
