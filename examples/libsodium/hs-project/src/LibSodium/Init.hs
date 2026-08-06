-- | Initialising libsodium, and querying its version.
module LibSodium.Init
  ( sodiumInit
  , libraryVersion
  ) where

import Control.Exception (throwIO)
import Control.Monad (when)

import HsBindgen.HighLevel (toHighLevel)
import HsBindgen.HighLevel.Auto (auto)

import Generated.Core.Safe (sodium_init)
import Generated.Version.Safe (sodium_library_version_major,
                               sodium_library_version_minor)
import LibSodium.Error (SodiumError (SodiumError))

-- | Initialise libsodium. Idempotent and thread-safe (libsodium 1.0.11+); call
-- once before any other operation. @sodium_init@ returns @-1@ on failure, @0@ on
-- first success, and @1@ if already initialised, so success is @>= 0@ (a naive
-- @== 0@ test is a latent bug).
sodiumInit :: IO ()
sodiumInit = do
  r <- sodium_init
  when (r < 0) $ throwIO (SodiumError "sodium_init" (fromIntegral r))

-- | The runtime library version, @(major, minor)@. Both getters take no arguments
-- and return an @int@, so @auto@ lifts them end to end with no marshaller. The
-- crypto operations below need more, because @auto@ cannot fill an output buffer.
libraryVersion :: IO (Int, Int)
libraryVersion = (,) <$> major <*> minor
  where
    major, minor :: IO Int
    major = toHighLevel sodium_library_version_major auto
    minor = toHighLevel sodium_library_version_minor auto
