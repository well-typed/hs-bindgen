-- | A fork of @tasty-golden" with additional features
--
-- New features:
--
-- * Report progress
-- * Skip tests
--
-- Intended for unqualified import.
module Test.Common.Util.Tasty.Golden (
    ActualValue(..)
  , goldenTestSteps
  ) where

import Control.DeepSeq (rnf)
import Control.Exception
import Data.Functor ((<&>))
import Data.IORef
import Data.Proxy
import System.IO.Error (isDoesNotExistError)

import Test.Tasty.Options
import Test.Tasty.Providers

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

data ActualValue a =
    -- | The value we want to test against the golden reference
    ActualValue a

    -- | We failed to create the actual value
    --
    -- This is a test failure.
  | ActualFailed String

    -- | Skip this test
    --
    -- In some cases we are unable to construct an actual value, without it
    -- being a test failure.
  | ActualSkipped String
  deriving stock (Show, Functor)

-- | A very general testing function.
goldenTestSteps ::
     TestName
     -- ^ Test name
  -> IO a
     -- ^ Get the golden correct value
     --
     -- Note that this action may be followed by the update function call.
     -- Therefore, this action should avoid reading the file lazily; otherwise,
     -- the file may remain half-open and the update action will fail.
  -> ((String -> IO ()) -> IO (ActualValue a))
     -- ^ Get the tested value, given a callback for reporting progress
  -> (a -> a -> IO (Maybe String))
     -- ^ Comparison function
     --
     -- If two values are the same, it should return 'Nothing'. If they are
     -- different, it should return an error that will be printed to the user.
     -- First argument is the golden value.
     --
     -- The function may use 'IO', for example, to launch an external @diff@
     -- command.
  -> (a -> IO ())
     -- ^ Update the golden file
  -> TestTree
goldenTestSteps t getGolden getActual comparison updateGolden =
    singleTest t GoldenSteps{..}

{-------------------------------------------------------------------------------
  Internals
-------------------------------------------------------------------------------}

data GoldenSteps = forall a. GoldenSteps {
      getGolden    :: IO a
    , getActual    :: (String -> IO ()) -> IO (ActualValue a)
    , comparison   :: a -> a -> IO (Maybe String)
    , updateGolden :: a -> IO ()
    }

-- | This option, when set to 'True', specifies that we should run in the
-- «accept tests» mode
newtype AcceptTests = AcceptTests Bool
  deriving (Eq, Ord)
instance IsOption AcceptTests where
    defaultValue = AcceptTests False
    parseValue = fmap AcceptTests . safeReadBool
    optionName = return "accept"
    optionHelp = return "Accept current results of golden tests"
    optionCLParser = flagCLParser (Just 'a') (AcceptTests True)

instance IsTest GoldenSteps where
    run opts golden progress = runGoldenSteps golden progress opts
    testOptions = return
        [ Option (Proxy :: Proxy AcceptTests)
        ]

runGoldenSteps :: GoldenSteps -> (Progress -> IO ()) -> OptionSet -> IO Result
runGoldenSteps GoldenSteps{..} progress opts = do
    msgsRef <- newIORef []
    let stepFn :: String -> IO ()
        stepFn msg = do
           progress (Progress msg 0)
           atomicModifyIORef msgsRef (\msgs -> (msg:msgs, ()))

    -- get actual value
    mbNew <- try $ getActual stepFn
    msgs  <- readIORef msgsRef <&> reverse

    let testPassedWith, testFailedWith :: String -> IO Result
        testPassedWith descr = return $ testPassed $ unlines (descr : msgs)
        testFailedWith descr = return $ testFailed $ unlines (descr : msgs)

    case mbNew of
      Left (e :: SomeException) ->
        case fromException @AsyncException e of
          Just e' -> throwIO e'
          Nothing -> return $ testFailed $ displayException e

      Right (ActualSkipped reason) ->
        testPassedWith $ "skipped: " ++ reason

      Right (ActualFailed err) ->
        testFailedWith err

      Right (ActualValue new) -> do
        mbRef <- try getGolden
        case mbRef of
          Left e
            | Just e' <- fromException e, isDoesNotExistError e' ->
                if accept then do
                  updateGolden new
                  testPassedWith "Golden file did not exist; created"
                else do
                  testFailedWith "Golden file does not exist"
            | Just _ <- fromException @AsyncException e ->
                throwIO e
            | Just _ <- fromException @IOError e ->
                throwIO e
            | otherwise -> do
                -- Other types of exceptions may be due to failing to decode the
                -- golden file. In that case, it makes sense to replace a broken
                -- golden file with the current version.
                updateGolden new
                testPassedWith $ concat [
                    "Accepted the new version. Was failing with exception: "
                  , displayException e
                  ]

          Right ref -> do
              result <- comparison ref new
              case result of
                Nothing ->
                  return $ testPassed $ unlines msgs
                Just _reason | accept -> do
                  -- test failed; accept the new version
                  updateGolden new
                  testPassedWith "Accepted the new version"
                Just reason -> do
                  -- Make sure that the result is fully evaluated and doesn't
                  -- depend on yet un-read lazy input
                  evaluate . rnf $ reason
                  testFailedWith reason
  where
    AcceptTests accept = lookupOption opts
