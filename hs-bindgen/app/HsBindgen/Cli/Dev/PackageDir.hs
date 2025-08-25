module HsBindgen.Cli.Dev.PackageDir (
    -- * CLI help
    info
      -- * Options
  , Opts(..)
  , parseOpts
    -- * Execution
  , exec
  ) where

import Control.Monad ((<=<))
import Data.Text qualified as Text
import Options.Applicative hiding (info)
import System.IO.Silently qualified as Silently

import Clang.Args
import Clang.LowLevel.Core
import HsBindgen.Clang
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Get package directory from libclang"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts

parseOpts :: Parser Opts
parseOpts = pure Opts

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: Opts -> IO ()
exec Opts = void . withTracer def $
    putStrLn . ("resource directory: " ++) . show <=< getResourceDir

-- TODO note side effect of newline
-- TODO warning no threads
getResourceDir :: Tracer IO ClangMsg -> IO (Maybe FilePath)
getResourceDir tracer = auxOut 0 0 >>= \case
    Just (out, numFillBytes) -> do
      let (mResourceDir, numPrintBytes) =
            case takeWhile isEOL <$> break isEOL out of
              (s, n)
                | null s    -> (Nothing, length n)
                | otherwise -> (Just s,  length s + length n)
          buffSize = length out
          numBytes = buffSize + buffSize - numPrintBytes - numFillBytes
      -- TODO debug trace buffer size?
      void . Silently.capture_ $ auxFillBuffer 1 numBytes
      -- TODO trace on Nothing
      return mResourceDir
    Nothing ->
      -- TODO trace abort
      return Nothing
  where
    auxOut :: Int -> Int -> IO (Maybe (String, Int))
    auxOut !n !numBytes
      | n == 0 = do
          (out, numBytes') <- Silently.capture $ do
            auxPrintResourceDir
            auxFillBuffer initN numBytesN
          if null out
            then auxOut initN numBytes'
            else return $ Just (out, numBytes')
      | n < maxN = do
          (out, numBytes') <- Silently.capture $ auxFillBuffer 1 numBytesN
          if null out
            then auxOut (n + 1) (numBytes + numBytes')
            else return $ Just (out, numBytes + numBytes')
      | otherwise = return Nothing

    initN, maxN, numBytesN :: Int
    initN     = 4
    maxN      = 32
    numBytesN = 1024

    auxPrintResourceDir :: IO ()
    auxPrintResourceDir = void $
      let args'       = def { clangArgsBefore = ["-print-resource-dir"] }
          clangSetup' = defaultClangSetup args' $ ClangInputMemory filename ""
      in  withClang' nullTracer clangSetup' $ const (return Nothing)

    auxFillBuffer :: Int -> Int -> IO Int
    auxFillBuffer reps numBytes = do
      void . withClang tracer clangSetup $ \unit -> do
        let t = Text.pack . concat . replicate reps $
              replicate numBytes '.' ++ "\n"
        file <- clang_getFile unit filenameT
        sloc <- clang_getLocation unit file 1 1
        bracket
          (clang_CXRewriter_create unit)
          clang_CXRewriter_dispose
          $ \rewriter -> do
              clang_CXRewriter_insertTextBefore rewriter sloc t
              clang_CXRewriter_writeMainFileToStdOut rewriter
        return Nothing
      return $ reps * (numBytes + 1)

    clangSetup :: ClangSetup
    clangSetup = defaultClangSetup def $ ClangInputMemory filename ""

    filename :: FilePath
    filename = "hs-bindgen-print-resource-dir.h"

    filenameT :: Text
    filenameT = Text.pack filename

    isEOL :: Char -> Bool
    isEOL = (`elem` ("\r\n" :: [Char]))
