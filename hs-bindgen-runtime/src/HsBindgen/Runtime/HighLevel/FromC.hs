-- | High-level marshalling: C-side return values to Haskell values.
--
-- 'FromC' is the dual of 'HsBindgen.Runtime.HighLevel.ToC'. It is /not/
-- functionally-dependent, so a single constrained instance can cover a
-- family of C types — for example,
--
-- > instance Integral cn => FromC cn Int where
-- >   fromC = fromIntegral
--
-- handles @CInt@, @CSize@, @CLong@ and friends in one go. The C side is
-- pinned by the @'HighLevel' ('IO' hs) ('IO' c)@ instance in
-- "HsBindgen.Runtime.HighLevel.Call".
--
-- Caveat: narrowing conversions silently lose information
-- (@CLLong -> Int@ on 32-bit, @Word32@ near max on 32-bit).
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Runtime.HighLevel.FromC qualified as FromC
module HsBindgen.Runtime.HighLevel.FromC (
    -- * The class
    FromC (..)
  ) where

import Foreign.C.Types (CBool, CDouble, CFloat)
import Foreign.Marshal.Utils qualified as Marshal

-- | Convert a C-side return value @c@ to a Haskell value @hs@.
class FromC c hs where
  fromC :: c -> hs

{-------------------------------------------------------------------------------
  Integer conversions (any Integral C type → Int / Word / Integer)
-------------------------------------------------------------------------------}

instance {-# OVERLAPPABLE #-} Integral cn => FromC cn Int where
  fromC = fromIntegral

instance {-# OVERLAPPABLE #-} Integral cn => FromC cn Word where
  fromC = fromIntegral

instance {-# OVERLAPPABLE #-} Integral cn => FromC cn Integer where
  fromC = fromIntegral

{-------------------------------------------------------------------------------
  Float / Bool conversions
-------------------------------------------------------------------------------}

instance FromC CFloat Float where
  fromC = realToFrac

instance FromC CDouble Double where
  fromC = realToFrac

instance FromC CBool Bool where
  fromC = Marshal.toBool

-- | Identity passthrough. 'OVERLAPPABLE' so the constrained instances
-- above win when they apply. Covers @IO CSize@, @IO CDouble@,
-- @IO ()@, etc.
instance {-# OVERLAPPABLE #-} FromC c c where
  fromC = id
