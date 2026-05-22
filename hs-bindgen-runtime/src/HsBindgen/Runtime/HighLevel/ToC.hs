-- | High-level marshalling: Haskell values to C-side representations.
--
-- A 'ToC' instance ties a Haskell type to its C-side /slot/ ('CSlot')
-- and the bracket that produces it ('withC'). Both defaults are
-- identity, so the minimal instance is one line:
--
-- > instance ToC CInt
--
-- == Built-in marshalling
--
-- @
-- Haskell type             C slot                            Bracket
-- -----------------------------------------------------------------
-- CChar, CInt, CSize, …    (itself)                          —
-- CFloat, CDouble, CBool   (itself)                          —
-- Bool                     CBool                             —
-- String                   PtrConst CChar                    withCString
-- ByteString               (PtrConst CChar, CSize)           useAsCStringLen
-- IncompleteArray a        (Ptr a, CSize)                    withElemPtr
-- ConstIncompleteArray a   (PtrConst a, CSize)               withElemPtr
-- Maybe a                  CSlot a                           (delegated)
-- @
--
-- Multi-slot Haskell values (the c2hs @&@ pattern, e.g.
-- 'ByteString') use a tuple 'CSlot'. The @Spread@ helper in
-- "HsBindgen.Runtime.HighLevel.Call" applies the tuple to the
-- curried C function automatically.
--
-- == Width-bearing integers
--
-- There is intentionally no @ToC Int@ instance: C has many integer
-- widths and a bare @Int@ doesn't say which one. Pick one of:
--
-- > -- Idiom 1: use the C width directly.
-- > hsRead :: CInt -> Ptr CChar -> CSize -> IO Int
--
-- > -- Idiom 2: a meaning-carrying newtype that picks the width.
-- > newtype Fd    = Fd    CInt  -- store the C value; withC just unwraps.
-- > newtype Bytes = Bytes Int   -- store Int; fromIntegral at boundary.
-- > instance ToC Fd    where type CSlot Fd    = CInt;  withC (Fd n)    k = k n
-- > instance ToC Bytes where type CSlot Bytes = CSize; withC (Bytes n) k = k (fromIntegral n)
-- > hsRead' :: Fd -> Ptr CChar -> Bytes -> IO Int
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Runtime.HighLevel.ToC qualified as ToC
module HsBindgen.Runtime.HighLevel.ToC (
    -- * The class
    ToC (..)
    -- * Const-pointer view of an array
  , ConstIncompleteArray (..)
    -- * Nullability
  , Nullable (..)
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Vector.Storable qualified as VS
import Foreign.C.String (withCString)
import Foreign.C.Types (CBool, CChar, CDouble, CFloat, CInt, CLong, CSChar,
                        CShort, CSize, CUChar, CUInt, CULong, CUShort)
import Foreign.Marshal.Utils qualified as Marshal
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable)

import HsBindgen.Runtime.IncompleteArray (IncompleteArray)
import HsBindgen.Runtime.IncompleteArray qualified as IA
import HsBindgen.Runtime.IsArray qualified as IsA
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

{-------------------------------------------------------------------------------
  Class
-------------------------------------------------------------------------------}

-- | Marshal a Haskell value into a C-side slot with bracketed
-- allocation semantics.
--
-- @'CSlot' a@ is the slot type (a tuple for multi-slot values).
-- 'withC' allocates any scratch, invokes the continuation with the
-- slot, and releases the scratch when the continuation returns.
--
-- Both defaults are identity (@'CSlot' a = a@, @'withC' x k = k x@),
-- so types that already match their C representation are one line:
--
-- > instance ToC CInt
class ToC a where
  type CSlot a :: Type
  type CSlot a = a

  withC :: a -> (CSlot a -> IO r) -> IO r

  default withC :: CSlot a ~ a => a -> (CSlot a -> IO r) -> IO r
  withC x k = k x

{-------------------------------------------------------------------------------
  Foreign.C.Types: identity passthrough via the defaults.
-------------------------------------------------------------------------------}

instance ToC CChar
instance ToC CSChar
instance ToC CUChar
instance ToC CShort
instance ToC CUShort
instance ToC CInt
instance ToC CUInt
instance ToC CLong
instance ToC CULong
instance ToC CSize
instance ToC CBool
instance ToC CFloat
instance ToC CDouble
instance ToC (Ptr a)
instance ToC (PtrConst a)

{-------------------------------------------------------------------------------
  Haskell-side types with C-side conversion
-------------------------------------------------------------------------------}

-- | Marshals to 'CBool' via 'Marshal.fromBool'. There is only one C
-- boolean type, so the mapping is unambiguous (unlike 'Int').
instance ToC Bool where
  type CSlot Bool = CBool
  withC b k = k (Marshal.fromBool b)

-- | NUL-terminated @const char*@ via 'withCString'. C must not retain
-- the pointer past the call.
instance ToC String where
  type CSlot String = PtrConst CChar
  withC s k = withCString s (k . PtrConst.unsafeFromPtr)

-- | Two-slot @(const char*, size_t)@ via 'BS.useAsCStringLen' — the
-- c2hs @&@ pattern.
instance ToC ByteString where
  type CSlot ByteString = (PtrConst CChar, CSize)
  withC bs k = BS.useAsCStringLen bs $ \(p, n) ->
    k (PtrConst.unsafeFromPtr p, fromIntegral n)

{-------------------------------------------------------------------------------
  IncompleteArray and its const variant
-------------------------------------------------------------------------------}

-- | Two-slot @(T*, size_t)@ for in/out array parameters. For
-- input-only @const T*@, use 'ConstIncompleteArray'.
instance Storable a => ToC (IncompleteArray a) where
  type CSlot (IncompleteArray a) = (Ptr a, CSize)
  withC arr k = IsA.withElemPtr arr $ \p ->
    k (p, fromIntegral (VS.length (IA.toVector arr)))

-- | Const-pointer view of an 'IncompleteArray' for input-only @const T*@.
--
-- > -- c_hash :: PtrConst Word8 -> CSize -> IO CULong
-- > hsHash :: ConstIncompleteArray Word8 -> IO CULong
-- > hsHash = hl c_hash
newtype ConstIncompleteArray a = ConstIncompleteArray (IncompleteArray a)
  deriving stock (Eq, Show)

instance Storable a => ToC (ConstIncompleteArray a) where
  type CSlot (ConstIncompleteArray a) = (PtrConst a, CSize)
  withC (ConstIncompleteArray arr) k = IsA.withElemPtr arr $ \p ->
    k (PtrConst.unsafeFromPtr p, fromIntegral (VS.length (IA.toVector arr)))

{-------------------------------------------------------------------------------
  Nullability
-------------------------------------------------------------------------------}

-- | A slot type with a designated null. Used by @'ToC' ('Maybe' a)@
-- to encode @Nothing@. Only pointer-shaped slots have instances.
class Nullable a where
  nullValue :: a

instance Nullable (Ptr a) where
  nullValue = nullPtr

instance Nullable (PtrConst a) where
  nullValue = PtrConst.unsafeFromPtr nullPtr

-- | Nullable pointer: @Nothing@ → null; @Just x@ delegates. The
-- @'Nullable' ('CSlot' a)@ constraint makes @'ToC' ('Maybe' 'Int')@
-- and @'ToC' ('Maybe' 'ByteString')@ deliberate compile errors —
-- there is no obvious null for either.
--
-- > hsCheckPresent :: Maybe String -> IO Bool
-- > hsCheckPresent = hl c_check_present
instance (ToC a, Nullable (CSlot a)) => ToC (Maybe a) where
  type CSlot (Maybe a) = CSlot a
  withC Nothing  k = k nullValue
  withC (Just x) k = withC x k
