-- | Parse @fixture.h@ and walk its AST (cursor kind, spelling, source line:col)
-- directly against the raw @Clang.LowLevel.FFI@: alloca / preallocate /
-- onHaskellHeap / copyToHaskellHeap plumbing, plus a hand-written
-- @foreign import ccall "wrapper"@ for the visitor.
--
-- The twin of app/WalkHigh.hs; the two must print byte-identical output.
module Main (main) where

import Control.Exception (Exception, SomeException, throwIO, try)
import Control.Monad (forM_)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Void (Void)
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (FunPtr, Ptr, castPtr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable (peek)
import System.Environment (getArgs)

import Clang.Enum.Simple (SimpleEnum (..), simpleToC)
import Clang.Internal.ByValue (OnHaskellHeap, R (..), copyToHaskellHeap,
                               onHaskellHeap, preallocate_)
import Clang.Internal.ConstPtr (ConstPtr (..))
import Clang.Internal.CXString ()
import Clang.LowLevel.Core.Enums (CXChildVisitResult (..))
import Clang.LowLevel.Core.Instances ()
import Clang.LowLevel.Core.Pointers (CXIndex, CXTranslationUnit)
import Clang.LowLevel.Core.Structs (CXCursor_, CXSourceLocation_, CXUnsavedFile)
import Clang.LowLevel.FFI

type HsVisitor = Ptr Void -> Ptr Void -> IO CInt

foreign import capi safe "hs_visit.h hs_visitChildren"
  c_hs_visitChildren :: R CXCursor_ -> FunPtr HsVisitor -> IO CUInt

foreign import ccall "wrapper"
  mkVisitor :: HsVisitor -> IO (FunPtr HsVisitor)

newtype ParseFailed = ParseFailed String
  deriving stock (Show)

instance Exception ParseFailed

main :: IO ()
main = do
    args <- getArgs
    let path = case args of
          (p : _) -> p
          []      -> "fixture.h"
    idx <- nowrapper_createIndex 0 0
    tu <- rawParse idx path ["-x", "c"]
    root <- preallocate_ (wrap_getTranslationUnitCursor tu)
    walk 0 root
    nowrapper_disposeTranslationUnit tu
    nowrapper_disposeIndex idx

rawParse :: CXIndex -> FilePath -> [String] -> IO CXTranslationUnit
rawParse idx path args =
    withCString path $ \cpath ->
      withMany withCString args $ \cargs ->
        withArray (map ConstPtr cargs) $ \argv ->
          alloca $ \out -> do
            st <-
              nowrapper_parseTranslationUnit2
                idx
                (ConstPtr cpath)
                (ConstPtr argv)
                (fromIntegral (length args))
                (castPtr nullPtr :: Ptr CXUnsavedFile)
                0
                0
                out
            case st of
              SimpleEnum 0 -> peek out
              SimpleEnum c -> throwIO (ParseFailed ("CXErrorCode " ++ show c))

walk :: Int -> OnHaskellHeap CXCursor_ -> IO ()
walk depth cur = do
    kids <- rawChildren cur
    forM_ kids $ \child -> do
      kind <- rawKindSpelling child
      name <- onHaskellHeap child $ \r -> preallocate_ @Text (wrap_getCursorSpelling r)
      (line, col) <- rawLineCol =<< onHaskellHeap child (\r -> preallocate_ (wrap_getCursorLocation r))
      T.putStrLn $
        T.concat
          [ T.replicate depth "  "
          , kind
          , " \""
          , name
          , "\" "
          , T.pack (show line)
          , ":"
          , T.pack (show col)
          ]
      walk (depth + 1) child

rawKindSpelling :: OnHaskellHeap CXCursor_ -> IO Text
rawKindSpelling child = do
    k <- onHaskellHeap child wrap_getCursorKind
    preallocate_ @Text (wrap_getCursorKindSpelling k)

rawLineCol :: OnHaskellHeap CXSourceLocation_ -> IO (Word, Word)
rawLineCol loc =
    onHaskellHeap loc $ \r ->
      alloca $ \pFile ->
        alloca $ \pLine ->
          alloca $ \pCol ->
            alloca $ \pOff -> do
              wrap_getSpellingLocation r pFile pLine pCol pOff
              line <- peek pLine
              col <- peek pCol
              pure (fromIntegral (line :: CUInt), fromIntegral (col :: CUInt))

rawChildren :: OnHaskellHeap CXCursor_ -> IO [OnHaskellHeap CXCursor_]
rawChildren cur = do
    ref <- newIORef []
    excRef <- newIORef Nothing
    let wrapped :: HsVisitor
        wrapped cursorPtr _parentPtr = do
          r <- try $ do
            child <- copyToHaskellHeap (castPtr cursorPtr :: Ptr CXCursor_)
            modifyIORef' ref (child :)
          case r of
            Right () -> pure (simpleToC CXChildVisit_Continue)
            Left e -> do
              writeIORef excRef (Just (e :: SomeException))
              pure (simpleToC CXChildVisit_Break)
    fp <- mkVisitor wrapped
    _ <- onHaskellHeap cur $ \r -> c_hs_visitChildren r fp
    freeHaskellFunPtr fp
    readIORef excRef >>= mapM_ throwIO
    reverse <$> readIORef ref
