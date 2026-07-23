-- | A small, idiomatic high-level binding to a slice of
-- [libsodium](https://libsodium.org/), built on hs-bindgen's generated low-level
-- bindings with the @ToHighLevel@ combinator library in @hs-bindgen-runtime@.
--
-- This is a stress-test, not a complete binding. It covers secret-key
-- authenticated encryption ("LibSodium.SecretBox") and Ed25519 signatures
-- ("LibSodium.Sign"); the write-up is in @FINDINGS.md@.
--
-- Start with 'LibSodium.Init.sodiumInit', which initialises the library. Pure
-- views of the deterministic operations are in "LibSodium.Pure" (import
-- qualified).
module LibSodium
  ( module LibSodium.Init
  , module LibSodium.Error
  , module LibSodium.Random
  , module LibSodium.SecretBox
  , module LibSodium.Sign
  ) where

import LibSodium.Error
import LibSodium.Init
import LibSodium.Random
import LibSodium.SecretBox
import LibSodium.Sign
