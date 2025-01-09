module HsBindgen.Runtime.ConstantArray (
    ConstantArray,
) where

import GHC.TypeNats
import Data.Proxy (Proxy (..))
import Foreign.Storable (Storable (..))
import Data.Vector.Storable qualified as VS

newtype ConstantArray (n :: Nat) a = CA (VS.Vector a)
type role ConstantArray nominal nominal

instance (Storable a, KnownNat n) => Storable (ConstantArray n a) where
    sizeOf _ = intVal (Proxy @n) * sizeOf (undefined :: a)

    alignment _ = alignment (undefined :: a)

    -- TODO: should (allocate and) memcpy
    -- NOTE: Vector.Storable interface is unsafe, as it's relies heavily
    -- on unsafePerformIO, and thus we'll need to be careful to avoid
    -- accidental aliasing.
    --
    -- Better to implement these as low-level IO code
    peek _ = fail "TODO: Storable ConstantArray peek"
    poke _ (CA _) = fail "TODO: Storable ConstantArray poke"

intVal :: forall n. KnownNat n => Proxy n -> Int
intVal p = fromIntegral (natVal p)
