{-# OPTIONS_GHC -Wno-orphans #-}

module Manual.Functions.HigherOrder (examples) where

import Control.Monad (when, (>=>))
import Foreign as F
import Foreign.C qualified as FC

import HsBindgen.Runtime.FunPtr

import Manual.Tools

import Callbacks
import Callbacks.Safe

{-------------------------------------------------------------------------------
  Instances for nested callback scaling function
-------------------------------------------------------------------------------}

-- Manual instances for nested callback scaling function
foreign import ccall "wrapper" mkScaleFunPtr ::
     (FC.CDouble -> FC.CInt -> IO FC.CDouble)
  -> IO (F.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble))

foreign import ccall "dynamic" fromScaleFunPtr ::
     F.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)
  -> FC.CDouble
  -> FC.CInt
  -> IO FC.CDouble

instance ToFunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble) where
  toFunPtr = mkScaleFunPtr

instance FromFunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble) where
  fromFunPtr = fromScaleFunPtr

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: IO ()
examples = do
    section "Callbacks (Passing Haskell functions to C callbacks)"

    withToFunPtr (FileOpenedNotification_Aux $ putStrLn "")
                 (onFileOpened . FileOpenedNotification)

    putStrLn ""
    withToFunPtr
        (ProgressUpdate_Aux $ \progress -> putStrLn $ "Progress: " ++ show progress ++ "%")
        (onProgressChanged . ProgressUpdate)

    putStrLn ""
    withToFunPtr
      (DataValidator_Aux $ \value -> do
        putStrLn $ "Validating: " ++ show value
        return $ if value > 0 then 1 else 0)
      $ (\validator -> do
          result1 <- validateInput validator 50
          result2 <- validateInput validator (-10)
          putStrLn $ "Validation results: " ++ show result1 ++ ", " ++ show result2
        ) . DataValidator

    putStrLn ""
    withToFunPtr
      (MeasurementReceived_Aux $ peek >=> print)
      (onNewMeasurement . MeasurementReceived)

    putStrLn ""
    subsection "Nested callbacks"

    alloca $ \measurementPtr -> do
      poke measurementPtr $ Measurement { measurement_value = 100.0, measurement_timestamp = 1.0 }

      transformerFunPtr <- toFunPtr $ \mPtr innerScaleFunPtr factor -> do
        m <- peek mPtr
        putStrLn $ "  Transforming measurement: " ++ show (measurement_value m) ++ ", factor=" ++ show factor
        newValue <- if innerScaleFunPtr == F.nullFunPtr
                    then do
                      putStrLn "  (Scaling function was NULL, using direct multiplication)"
                      return $ measurement_value m * fromIntegral factor
                    else do
                      putStrLn "  (Using provided scaling function)"
                      let scaleFun = fromFunPtr innerScaleFunPtr
                      scaleFun (measurement_value m) factor
        poke mPtr $ m { measurement_value = newValue }
        putStrLn $ "  New value: " ++ show newValue

      putStrLn "Testing transformMeasurement with nested callbacks:"
      transformMeasurement measurementPtr transformerFunPtr

      finalMeasurement <- peek measurementPtr
      putStrLn $ "Final measurement: " ++ show finalMeasurement

    putStrLn ""
    alloca $ \measurementPtr -> do
      poke measurementPtr $ Measurement { measurement_value = 42.0, measurement_timestamp = 2.0 }

      handlerFunPtr <- toFunPtr $ \(mPtr :: Ptr Measurement)
                                   (notify :: FileOpenedNotification)
                                   (priority :: FC.CInt) -> do
        m <- peek mPtr
        putStrLn $ "  Handler called: value=" ++ show (measurement_value m)
                ++ ", priority=" ++ show priority
        if un_FileOpenedNotification notify /= F.nullFunPtr
          then do
            let (FileOpenedNotification_Aux notifyFn) = fromFunPtr (un_FileOpenedNotification notify)
            notifyFn
          else putStrLn "  (Notification callback was NULL)"

      putStrLn "Testing processWithCallbacks with multiple callbacks:"
      processWithCallbacks handlerFunPtr

    putStrLn ""
    subsection "Callbacks with structs and unions"

    putStrLn ""
    -- Test MeasurementHandler struct (struct with function pointers)
    putStrLn "Testing registerHandler (struct with multiple function pointers):"
    alloca $ \handlerPtr -> do
      onReceivedFunPtr <- toFunPtr $ \mPtr -> do
        m <- peek mPtr
        putStrLn $ "  [onReceived] Measurement: " ++ show (measurement_value m)

      validateFunPtr <- toFunPtr $ \mPtr -> do
        m <- peek mPtr
        let isValid = measurement_value m > 0
        putStrLn $ "  [validate] Measurement value " ++ show (measurement_value m)
                ++ " is " ++ (if isValid then "valid" else "invalid")
        return $ if isValid then 1 else 0

      onErrorFunPtr <- toFunPtr $ \errorCode -> do
        putStrLn $ "  [onError] Error code: " ++ show errorCode

      poke handlerPtr $ MeasurementHandler
        { measurementHandler_onReceived = onReceivedFunPtr
        , measurementHandler_validate = validateFunPtr
        , measurementHandler_onError = onErrorFunPtr
        }

      registerHandler handlerPtr

    putStrLn ""
    -- Test DataPipeline struct (struct with nested callback types)
    putStrLn "Testing executePipeline (struct with nested function pointer types):"
    alloca $ \measurementPtr -> do
      poke measurementPtr $ Measurement { measurement_value = 50.0, measurement_timestamp = 3.0 }

      alloca $ \pipelinePtr -> do
        preProcessFunPtr <- toFunPtr $ \mPtr validator -> do
          m <- peek mPtr
          putStrLn $ "  [preProcess] Processing measurement: " ++ show (measurement_value m)
          putStrLn $ "  [preProcess] Validator present: " ++ show (un_DataValidator validator /= F.nullFunPtr)

        processFunPtr <- toFunPtr $ \mPtr -> do
          m <- peek mPtr
          let newValue = measurement_value m * 2
          poke mPtr $ m { measurement_value = newValue }
          putStrLn $ "  [process] Doubled value to: " ++ show newValue

        postProcessFunPtr <- toFunPtr $ \mPtr progressUpdate -> do
          m <- peek mPtr
          putStrLn $ "  [postProcess] Final value: " ++ show (measurement_value m)
          putStrLn $ "  [postProcess] ProgressUpdate present: " ++ show (un_ProgressUpdate progressUpdate /= F.nullFunPtr)

        poke pipelinePtr $ DataPipeline
          { dataPipeline_preProcess = preProcessFunPtr
          , dataPipeline_process = processFunPtr
          , dataPipeline_postProcess = postProcessFunPtr
          }

        executePipeline measurementPtr pipelinePtr
        finalMeasurement <- peek measurementPtr
        putStrLn $ "  Final measurement after pipeline: " ++ show finalMeasurement

    putStrLn ""
    -- Test Processor struct (union with function pointers)
    putStrLn "Testing runProcessor (struct with union of function pointers):"
    alloca $ \measurementPtr -> do
      poke measurementPtr $ Measurement { measurement_value = 25.0, measurement_timestamp = 4.0 }

      -- Test MODE_SIMPLE
      alloca $ \processorPtr -> do
        simpleFunPtr <- toFunPtr $ \mPtr -> do
          m <- peek mPtr
          putStrLn $ "  [simple] Processing: " ++ show (measurement_value m)

        let callback = set_processorCallback_simple simpleFunPtr
        poke processorPtr $ Processor
          { processor_mode = MODE_SIMPLE
          , processor_callback = callback
          }

        putStrLn "  Testing MODE_SIMPLE:"
        runProcessor measurementPtr processorPtr

      -- Test MODE_VALIDATED
      alloca $ \processorPtr -> do
        validatedFunPtr <- toFunPtr $ \mPtr validator -> do
          m <- peek mPtr
          putStrLn $ "  [withValidator] Processing: " ++ show (measurement_value m)
          putStrLn $ "  [withValidator] Validator present: " ++ show (un_DataValidator validator /= F.nullFunPtr)

        let callback = set_processorCallback_withValidator validatedFunPtr
        poke processorPtr $ Processor
          { processor_mode = MODE_VALIDATED
          , processor_callback = callback
          }

        putStrLn "  Testing MODE_VALIDATED:"
        runProcessor measurementPtr processorPtr

    putStrLn ""
    subsection "Third-order callbacks"

    -- Test processMeasurementWithValidation (deeply nested callbacks)
    putStrLn "Testing processMeasurementWithValidation (third-order function):"
    alloca $ \measurementPtr -> do
      poke measurementPtr $ Measurement { measurement_value = 75.0, measurement_timestamp = 5.0 }

      processorFunPtr <- toFunPtr $ \mPtr transformerFunPtr validatorFunPtr -> do
        m <- peek mPtr
        putStrLn $ "  [processor] Processing measurement: " ++ show (measurement_value m)
        putStrLn $ "  [processor] Transformer present: " ++ show (transformerFunPtr /= F.nullFunPtr)
        putStrLn $ "  [processor] Validator present: " ++ show (un_DataValidator validatorFunPtr /= F.nullFunPtr)

        when (transformerFunPtr /= F.nullFunPtr) $ do
          let transformerFun = fromFunPtr transformerFunPtr
          transformerFun mPtr validatorFunPtr 10

      processMeasurementWithValidation measurementPtr processorFunPtr

      finalMeasurement <- peek measurementPtr
      putStrLn $ "  Final measurement: " ++ show finalMeasurement
