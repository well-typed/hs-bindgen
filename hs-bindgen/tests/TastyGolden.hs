-- | A fork of @tasty-golden" allowing to report progress. 'goldenTest' with 'testCaseSteps'.
module TastyGolden (
    goldenTestSteps,
) where

import Control.DeepSeq (rnf)
import Control.Exception (SomeException, AsyncException, Exception (..), throwIO, try, evaluate)
import Data.Functor ((<&>))
import Data.IORef (newIORef, atomicModifyIORef, readIORef)
import Data.Proxy (Proxy (..))
import System.IO.Error (isDoesNotExistError)

import Test.Tasty.Options
import Test.Tasty.Providers

-- | A very general testing function.
goldenTestSteps
    :: TestName -- ^ test name
    -> (IO a)
      -- ^ get the golden correct value
      --
      -- Note that this action may be followed by the update function call.
      --
      -- Therefore, this action *should avoid* reading the file lazily;
      -- otherwise, the file may remain half-open and the update action will
      -- fail.
    -> ((String -> IO ()) -> IO a) -- ^ get the tested value
    -> (a -> a -> IO (Maybe String))
      -- ^ comparison function.
      --
      -- If two values are the same, it should return 'Nothing'. If they are
      -- different, it should return an error that will be printed to the user.
      -- First argument is the golden value.
      --
      -- The function may use 'IO', for example, to launch an external @diff@
      -- command.
    -> (a -> IO ())
      -- ^ update the golden file
    -> TestTree
goldenTestSteps t golden test cmp upd = singleTest t $ GoldenSteps golden test cmp upd

data GoldenSteps where
    GoldenSteps
        :: IO a
        -> ((String -> IO ()) -> IO a)
        -> (a -> a -> IO (Maybe String))
        -> (a -> IO ())
        -> GoldenSteps

-- | This option, when set to 'True', specifies that we should run in the
-- «accept tests» mode
newtype AcceptTests = AcceptTests Bool
  deriving (Eq, Ord)
instance IsOption AcceptTests where
    defaultValue = AcceptTests False
    parseValue = fmap AcceptTests . safeReadBool
    optionName = return "accept"
    optionHelp = return "Accept current results of golden tests"
    optionCLParser = flagCLParser Nothing (AcceptTests True)

instance IsTest GoldenSteps where
    run opts golden progress = runGoldenSteps golden progress opts
    testOptions = return
        [ Option (Proxy :: Proxy AcceptTests)
        ]

runGoldenSteps :: GoldenSteps -> (Progress -> IO ()) -> OptionSet -> IO Result
runGoldenSteps (GoldenSteps getGolden getTested cmp update) progress opts = do
    let AcceptTests accept = lookupOption opts

    msgsRef <- newIORef []

    let stepFn :: String -> IO ()
        stepFn msg = do
           progress (Progress msg 0)
           atomicModifyIORef msgsRef (\msgs -> (msg:msgs, ()))

    -- get actual value
    mbNew <- try $ getTested stepFn
    msgs <- readIORef msgsRef <&> reverse

    case mbNew of
        Left e
            | Just _ <- fromException @AsyncException e -> throwIO e
            | otherwise ->
                return $ testFailed $ displayException (e :: SomeException)

        Right new -> do
            -- get expected value
            mbRef <- try getGolden
            case mbRef of
                Left e
                    | Just e' <- fromException e, isDoesNotExistError e' ->
                        if accept
                        then do
                            update new
                            return $ testPassed $ unlines $ "Golden file did not exist; created" : msgs
                        else do
                            return $ testFailed $ unlines $ "Golden file does not exist" : msgs

                    | Just _ <- fromException @AsyncException e -> throwIO e
                    | Just _ <- fromException @IOError e        -> throwIO e

                    | otherwise -> do
                        -- Other types of exceptions may be due to failing to decode the
                        -- golden file. In that case, it makes sense to replace a broken
                        -- golden file with the current version.
                        update new
                        return $ testPassed $ unlines $
                            "Accepted the new version. Was failing with exception:" :
                            displayException e :
                            msgs

                Right ref -> do
                    result <- cmp ref new

                    case result of
                        Nothing -> do
                            return $ testPassed $ unlines msgs

                        Just _reason | accept -> do
                            -- test failed; accept the new version
                            update new
                            return $ testPassed $ unlines $ "Accepted the new version" : msgs

                        Just reason -> do
                            -- Make sure that the result is fully evaluated and doesn't depend
                            -- on yet un-read lazy input
                            evaluate . rnf $ reason
                            return $ testFailed $ unlines $ reason : msgs
