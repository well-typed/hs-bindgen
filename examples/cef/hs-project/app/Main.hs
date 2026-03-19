{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Foreign.C.String (newCString)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (withArray0)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke, sizeOf)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitSuccess, exitWith)

import HsBindgen.Runtime.Internal.FunPtr qualified as FunPtr
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import CEF.ApiHash.Safe (cef_api_hash, cef_api_version)
import CEF.App (C_Cef_settings_t, Cef_main_args_t (..))
import CEF.App.Safe (cef_execute_process, cef_initialize, cef_shutdown)
import CEF.CommandLine (Cef_command_line_t (..))
import CEF.CommandLine.Safe (cef_command_line_create)

-- | Initialize CEF in headless mode, create a command line object, and exercise
-- its vtable methods. This validates that the generated bindings work
-- end-to-end: FFI calls, struct layout, and function pointer dispatch.
main :: IO ()
main = do
    -- Configure the API version in libcef.so. This MUST happen before any
    -- other CEF function calls, otherwise CppToC struct wrappers will crash.
    _ <- cef_api_hash 14500 0

    -- Pass real argc/argv plus headless flags. CEF re-executes the same binary
    -- for renderer/GPU subprocesses with --type=renderer etc.
    progName <- getProgName
    args <- getArgs
    let allArgs = progName : "--ozone-platform=headless" : "--disable-gpu" : args
    cArgs <- mapM newCString allArgs
    withArray0 nullPtr cArgs $ \argv ->
      alloca $ \mainArgsPtr -> do
        poke mainArgsPtr (Cef_main_args_t (fromIntegral (length allArgs)) argv)
        let constMainArgs = PtrConst.unsafeFromPtr mainArgsPtr

        -- Handle CEF subprocess model. Returns >= 0 for subprocesses.
        exitCode <- cef_execute_process constMainArgs nullPtr nullPtr
        if exitCode >= 0
            then if exitCode == 0 then exitSuccess
                                  else exitWith (ExitFailure (fromIntegral exitCode))
            else do
                -- Zero-initialize settings, set required fields
                let settingsSize = sizeOf (undefined :: C_Cef_settings_t)
                allocaBytes settingsSize $ \(settingsPtr :: Ptr C_Cef_settings_t) -> do
                    fillBytes settingsPtr 0 settingsSize
                    poke settingsPtr.size (fromIntegral settingsSize)
                    poke settingsPtr.no_sandbox 1

                    let constSettings = PtrConst.unsafeFromPtr settingsPtr
                    initResult <- cef_initialize constMainArgs constSettings nullPtr nullPtr

                    if initResult /= 1
                        then putStrLn $ "CEF initialization failed (code " ++ show initResult ++ ")"
                        else do
                            putStrLn "CEF initialized successfully."

                            -- Verify API version was configured correctly
                            ver <- cef_api_version
                            putStrLn $ "  API version: " ++ show ver

                            -- Create a command line object via the C API
                            cmdLine <- cef_command_line_create
                            assertNonNull "cef_command_line_create" cmdLine

                            -- Peek the struct to access the vtable
                            obj <- peek cmdLine

                            -- Call is_valid via vtable function pointer dispatch
                            let isValid = FunPtr.fromFunPtr obj.is_valid
                            valid <- isValid cmdLine
                            putStrLn $ "  is_valid: " ++ show valid

                            -- Call has_switches via vtable dispatch
                            let hasSwitches = FunPtr.fromFunPtr obj.has_switches
                            switches <- hasSwitches cmdLine
                            putStrLn $ "  has_switches (empty): " ++ show switches

                            putStrLn "All CEF binding calls succeeded."
                            cef_shutdown
                            putStrLn "CEF shutdown complete."
  where
    assertNonNull :: String -> Ptr a -> IO ()
    assertNonNull name ptr
        | ptr == nullPtr = fail $ name ++ " returned NULL"
        | otherwise      = putStrLn $ "  " ++ name ++ ": OK"
