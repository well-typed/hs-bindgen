{-# LANGUAGE UndecidableInstances #-}

-- | Bare-bones support for blocks
--
-- TODO: Ideally we would at least support @Block_copy@ and @Block_release@.
-- This would be easy to do, but would mean that @hs-bindgen-runtime@ would
-- then depend on the runtime (@libblocksruntime@).
module HsBindgen.Runtime.Block (
    Block(..)
  ) where

import Foreign (Ptr)

import HsBindgen.Runtime.HasFFIType (HasFFIType, ViaNewtype (..))

{-------------------------------------------------------------------------------
  Definition

  We expose the definition of 'Block' so that it can appear in foreign imports.
-------------------------------------------------------------------------------}

-- | Block
--
-- See <https://clang.llvm.org/docs/BlockLanguageSpec.html>
--
-- The type index is the type of the bloc, for example:
--
-- > typedef int(^VarCounter)(int increment);
--
-- corresponds to
--
-- > newtype VarCounter = VarCounter (Block (CInt -> IO CInt))
newtype Block t = Block (Ptr ())

deriving via ViaNewtype (Ptr ()) instance HasFFIType (Block t)
