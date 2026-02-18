module HsBindgen.Clang.Macos (
    -- * Types
    SdkPath
    -- * Trace messages
  , MacosMsg(..)
    -- * API
  , checkMacosEnv
  ) where

import Control.Exception (Exception (displayException))
import Control.Monad ((<=<))
import System.Environment qualified as Env
import System.Info qualified
import System.Process qualified as Proc
import Text.SimplePrettyPrint ((<+>))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Imports
import HsBindgen.Util.Process
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | SDK path
type SdkPath = FilePath

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data MacosMsg =
    MacosEnvAlreadySet SdkPath
  | MacosEnvNotSet
  | MacosXcrunRun
  | MacosXcrunUnexpected String
  | MacosXcrunIOError IOError
  | MacosEnvSet SdkPath
  deriving stock (Show)

instance PrettyForTrace MacosMsg where
  prettyForTrace = \case
    MacosEnvAlreadySet path ->
      PP.string envName <+> "already set:" <+> PP.string (show path)
    MacosEnvNotSet ->
      PP.string envName <+> "not set"
    MacosXcrunRun ->
      PP.hang ("Running xcrun to determine" <+> PP.string envName) 2 $
        "Command:" <+> cmdSpecPrettyForTrace xcrunCmd
    MacosXcrunUnexpected s ->
      PP.hangs ("Unable to determine" <+> PP.string envName) 2 [
          "Command:" <+> cmdSpecPrettyForTrace xcrunCmd
        , "Unexpected output:" <+> PP.string (show s)
        ]
    MacosXcrunIOError e ->
      PP.hangs ("Unable to determine" <+> PP.string envName) 2 [
          "Command:" <+> cmdSpecPrettyForTrace xcrunCmd
        , "IO error:" <+> PP.string (displayException e)
        ]
    MacosEnvSet path ->
      "Setting"
        <+> PP.string (envName ++ ":")
        <+> PP.string (show path)

instance IsTrace Level MacosMsg where
  getDefaultLogLevel = \case
    MacosEnvAlreadySet{}   -> Debug
    MacosEnvNotSet         -> Debug
    MacosXcrunRun          -> Info
    MacosXcrunUnexpected{} -> Warning
    MacosXcrunIOError{}    -> Warning
    MacosEnvSet{}          -> Notice

  getSource  = const HsBindgen
  getTraceId = const "macos"

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Check @SDKROOT@ environment variable on macOS
--
-- If @SDKROOT@ is not set, attempt to determine it with @xcrun@ and set it when
-- successful.
checkMacosEnv :: Tracer MacosMsg -> IO ()
checkMacosEnv tracer = when (System.Info.os == "darwin") $
    getEnv tracer >>= \case
      Just{}  -> return ()
      Nothing -> getXcrunSdkPath tracer >>= \case
        Nothing   -> return ()
        Just path -> setEnv tracer path

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

envName :: String
envName = "SDKROOT"

getEnv :: Tracer MacosMsg -> IO (Maybe SdkPath)
getEnv tracer = do
    mx <- Env.lookupEnv envName
    traceWith tracer $ maybe MacosEnvNotSet MacosEnvAlreadySet mx
    return mx

setEnv :: Tracer MacosMsg -> SdkPath -> IO ()
setEnv tracer path = do
    traceWith tracer (MacosEnvSet path)
    Env.setEnv envName path

--------------------------------------------------------------------------------

xcrunCmd :: Proc.CmdSpec
xcrunCmd = Proc.RawCommand "xcrun" ["--sdk", "macosx", "--show-sdk-path"]

getXcrunSdkPath :: Tracer MacosMsg -> IO (Maybe SdkPath)
getXcrunSdkPath tracer = do
    traceWith tracer MacosXcrunRun
    checkOutput
      tracer
      MacosXcrunUnexpected
      MacosXcrunIOError
      (parseNonEmpty <=< parseSingleLine)
      (cmdSpecReadAction xcrunCmd)
