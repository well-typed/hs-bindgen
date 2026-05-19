{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Control.Exception (bracket_)
import Control.Monad (when)
import Foreign qualified
import Foreign.C qualified as C
import System.Exit (exitFailure)

import HsBindgen.Runtime.PtrConst qualified as PtrConst

import Generated.Yaml qualified as Yaml
import Generated.Yaml.Safe qualified as Yaml

example :: String
example = unlines
  [ "# just a simple YAML example"
  , "string: some string"
  , "number: 12345"
  , "array:"
  , "- one"
  , "- two"
  , "- three"
  ]

-- print the parsed events of the example document
main :: IO ()
main = do
  withParser $ \parserPtr ->
    withInputString parserPtr example $ do
      Foreign.alloca $ \eventPtr ->
        go parserPtr eventPtr
  where
    go parserPtr eventPtr = do
      res <- Yaml.yaml_parser_parse parserPtr eventPtr
      when (res == 0) $ do
        errType <- Foreign.peek (parserPtr.error)
        putStrLn ("yaml_parser_parse: " ++ show res ++ ", error type: " ++ show errType)
        exitFailure
      eventType <- Foreign.peek eventPtr.type'
      when (eventType /= Yaml.YAML_NO_EVENT) $ do
        printEvent eventPtr
        go parserPtr eventPtr

printEvent :: Foreign.Ptr Yaml.Yaml_event_t -> IO ()
printEvent eventPtr = do
  eventType <- Foreign.peek eventPtr.type'
  case eventType of
    Yaml.YAML_SCALAR_EVENT -> do
      -- we know this is a scalar event, so we confidently assume that its data
      -- holds the `.scalar` variant of the union.
      let scalar = eventPtr.data'.scalar :: Foreign.Ptr Yaml.Yaml_event_s_data_scalar
      valuePtr <- Foreign.peek scalar.value
      value <- C.peekCString (Foreign.castPtr @Yaml.Yaml_char_t @C.CChar valuePtr)
      putStrLn (show eventType ++ ": " ++ value)
    _ -> do
      print eventType

withParser :: (Foreign.Ptr Yaml.Yaml_parser_t -> IO a) -> IO a
withParser k =
  Foreign.alloca $ \parserPtr ->
    bracket_ (initParser parserPtr) (deleteParser parserPtr) $
      k parserPtr
  where
    initParser parserPtr = do
      res <- Yaml.yaml_parser_initialize parserPtr
      when (res == 0) $ do
        errType <- Foreign.peek (parserPtr.error)
        putStrLn ("yaml_parser_initialize: " ++ show res ++ ", error type: " ++ show errType)
        exitFailure

    deleteParser parserPtr =
      Yaml.yaml_parser_delete parserPtr

withInputString :: Foreign.Ptr Yaml.Yaml_parser_t -> String -> IO a -> IO a
withInputString parserPtr input k =
  C.withCStringLen input $ \(str, len) -> do
    let str' = PtrConst.unsafeFromPtr (Foreign.castPtr @C.CChar @C.CUChar str)
    Yaml.yaml_parser_set_input_string parserPtr str' (fromIntegral len)
    k
