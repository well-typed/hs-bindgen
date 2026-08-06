-- | A small, idiomatic high-level binding to a slice of
-- [libsodium](https://libsodium.org/), built on hs-bindgen's generated low-level
-- bindings with the @ToHighLevel@ combinators from @hs-bindgen-highlevel@.
--
-- It covers secret-key authenticated encryption ("LibSodium.SecretBox") and
-- Ed25519 signatures ("LibSodium.Sign"). Everything else in libsodium is left
-- out: this exists to exercise the combinators against a buffer-oriented C API,
-- not to be a usable libsodium binding.
--
-- Start with 'LibSodium.Init.sodiumInit', which initialises the library.
--
-- After that, the 'IO' left in these types is the 'IO' that is really there. The
-- deterministic operations (encrypting, opening, signing, verifying, deriving a
-- keypair from a seed) are pure functions of their arguments and are typed that way;
-- only key and nonce generation, which read the CSPRNG, stay in 'IO'. Their specs are
-- closed with 'HsBindgen.HighLevel.toHighLevelPure', which is
-- 'HsBindgen.HighLevel.toHighLevel' with the 'IO' taken off the result.
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
