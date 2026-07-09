{-# LANGUAGE ScopedTypeVariables #-}

-- | Reusable marshallers that plug into the @ToHighLevel@ combinators for the
-- shapes libgit2 uses everywhere: managed-handle out-parameters, handles as
-- @T *@ / @const T *@ arguments, by-value @git_oid@ in and out, borrowed C
-- strings, byte buffers, and structs passed as @const@ pointers.
--
-- The "Proposed combinators" section at the end (`asConstArg`, `asMutableArg`,
-- `fixed`) are candidates for the runtime itself; FINDINGS.md explains why. They
-- are written here so the example can use them today.
--
module LibGit2.Marshal
  ( -- * Handles
    handleIn
  , handleInC
  , outHandle
    -- * Object ids
  , oidIn
  , oidInC
  , oidOut
  , peekOidConst
    -- * Strings and buffers
  , textIn
  , textInPtr
  , bufferIn
  , peekTextConst
  , peekText
    -- * Borrowed-pointer accessors
  , borrowedText
  , borrowedOid
  , borrowedScalar
    -- * Structs and constants
  , asArgumentC
  , nullConst
    -- * Proposed combinators (candidates for hs-bindgen-runtime)
  , asConstArg
  , asMutableArg
  , fixed
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as BSU
import Data.Text (Text)
import Data.Text qualified as T
import Foreign.C.String (peekCString, withCString)
import Foreign.C.Types (CChar, CSize)
import Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)

import HsBindgen.Runtime.HighLevel (ToHighLevel, input, resultIO, resultPure,
                                    scratch, toHighLevel)
import HsBindgen.Runtime.HighLevel.Internal.Threading (ThreadIn)
import HsBindgen.Runtime.HighLevel.Marshaller (Marshal (..), MarshalStruct,
                                               Unmarshaller, at, bracket,
                                               unmarshalOut, unmarshalOutWith,
                                               withStruct)
import HsBindgen.Runtime.HighLevel.Marshaller.Utils (withCStringIn)
import HsBindgen.Runtime.Marshal (StaticSize, WriteRaw)
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import Generated.Oid (Git_oid)
import LibGit2.Types (Handle (..), Oid (..), withHandle)

{-------------------------------------------------------------------------------
  Handles
-------------------------------------------------------------------------------}

-- | Pass a handle as a non-@const@ @T *@ argument. The handle stays alive across
-- the call.
handleIn :: Handle h => Marshal h (Ptr (CRep h) -> lo) lo
handleIn = bracket withHandle

-- | Pass a handle as a @const T *@ argument. This is just 'handleIn' with its
-- pointer retagged @const@ by 'asConstArg' (see below): one marshaller, two C
-- argument shapes.
handleInC :: Handle h => Marshal h (PtrConst (CRep h) -> lo) lo
handleInC = asConstArg handleIn

-- | The constructor out-parameter @git_X **out@: allocate the slot, let the call
-- fill it, then wrap the @git_X *@ in a 'Foreign.ForeignPtr.ForeignPtr' with its
-- @git_X_free@ finaliser, so the handle frees itself at GC.
outHandle
  :: forall h. Handle h
  => FinalizerPtr (CRep h)
  -> Unmarshaller (Ptr (Ptr (CRep h))) h
outHandle fin = unmarshalOutWith alloca $ \pp -> do
  p <- peek pp
  fromFP <$> newForeignPtr fin p

{-------------------------------------------------------------------------------
  Object ids
-------------------------------------------------------------------------------}

-- | Marshal an 'Oid' as a non-@const@ @git_oid *@ (pointer to a temporary copy).
oidIn :: Marshal Oid (Ptr Git_oid -> lo) lo
oidIn = bracket (\(Oid g) k -> with g k)

-- | Marshal an 'Oid' as a @const git_oid *@ (the @const@ form of 'oidIn').
oidInC :: Marshal Oid (PtrConst Git_oid -> lo) lo
oidInC = asConstArg oidIn

-- | An out-parameter @git_oid *@ the callee fills.
oidOut :: Unmarshaller (Ptr Git_oid) Oid
oidOut = unmarshalOut (pure . Oid)

-- | Copy a borrowed @const git_oid *@ into an 'Oid'.
peekOidConst :: PtrConst Git_oid -> IO Oid
peekOidConst = fmap Oid . peek . PtrConst.unsafeToPtr

{-------------------------------------------------------------------------------
  Strings and buffers
-------------------------------------------------------------------------------}

-- | Marshal 'Text' as a NUL-terminated @const char *@.
textIn :: Marshal Text (PtrConst CChar -> lo) lo
textIn = at T.unpack withCStringIn

-- | Marshal 'Text' as a NUL-terminated non-@const@ @char *@ (e.g. a
-- @git_signature@ name/email field).
textInPtr :: Marshal Text (Ptr CChar -> lo) lo
textInPtr = bracket (\t k -> withCString (T.unpack t) k)

-- | Marshal a 'ByteString' as a @(const void *, size_t)@ pair. The pointee is
-- left polymorphic so it unifies with @void@ at the call site. Uses
-- 'BSU.unsafeUseAsCStringLen' (no copy): the C callee takes an explicit length
-- and does not retain the pointer.
bufferIn :: Marshal ByteString (PtrConst void -> CSize -> lo) lo
bufferIn = Marshal $ \bs lo k ->
  BSU.unsafeUseAsCStringLen bs $ \(p, n) ->
    k (lo (PtrConst.unsafeFromPtr (castPtr p)) (fromIntegral n))

-- | Copy a borrowed @const char *@ into 'Text' (NULL becomes empty).
peekTextConst :: PtrConst CChar -> IO Text
peekTextConst = peekText . PtrConst.unsafeToPtr

-- | Copy a borrowed @char *@ into 'Text' (NULL becomes empty).
peekText :: Ptr CChar -> IO Text
peekText p
  | p == nullPtr = pure T.empty
  | otherwise    = T.pack <$> peekCString p

{-------------------------------------------------------------------------------
  Borrowed-pointer accessors

  Several wrappers share the exact same spec: take a handle as @const T *@, call
  an accessor that returns a borrowed pointer, copy it out. These name that shape
  once so each accessor is a single application.
-------------------------------------------------------------------------------}

-- | A @const T *@ accessor returning a borrowed @const char *@, copied to 'Text'.
borrowedText :: Handle h => (PtrConst (CRep h) -> IO (PtrConst CChar)) -> h -> IO Text
borrowedText = toHighLevel (input handleInC $ resultIO peekTextConst)

-- | A @const T *@ accessor returning a borrowed @const git_oid *@, copied to 'Oid'.
borrowedOid :: Handle h => (PtrConst (CRep h) -> IO (PtrConst Git_oid)) -> h -> IO Oid
borrowedOid = toHighLevel (input handleInC $ resultIO peekOidConst)

-- | A @const T *@ accessor returning a C scalar, converted with 'fromIntegral'.
borrowedScalar :: (Handle h, Integral c, Num n) => (PtrConst (CRep h) -> IO c) -> h -> IO n
borrowedScalar = toHighLevel (input handleInC $ resultPure fromIntegral)

{-------------------------------------------------------------------------------
  Structs and constants
-------------------------------------------------------------------------------}

-- | Drop a 'MarshalStruct' into a @const T *@ argument position (the @const@
-- counterpart of the runtime's @asArgument@, which only fills a non-@const@ @Ptr@).
asArgumentC
  :: (StaticSize s, WriteRaw s)
  => MarshalStruct hi s
  -> Marshal hi (PtrConst s -> lo) lo
asArgumentC sm = Marshal $ \hi lo k -> withStruct sm hi (\p -> k (lo (PtrConst.unsafeFromPtr p)))

-- | A NULL @const@ pointer, for optional/absent C arguments.
nullConst :: PtrConst a
nullConst = PtrConst.unsafeFromPtr nullPtr

{-------------------------------------------------------------------------------
  Proposed combinators (candidates for hs-bindgen-runtime)
-------------------------------------------------------------------------------}

-- | Retag a marshaller that fills a @Ptr a@ argument so it fills a @PtrConst a@
-- argument instead. @PtrConst@ is a newtype over @Ptr@, so this is a coercion at
-- the boundary: one marshaller serves both C argument shapes, instead of two
-- near-identical definitions. (@handleInC = asConstArg handleIn@.)
--
-- The direction matters: @asConstArg@ (mutable -> const) is always safe, because a
-- @const T *@ promises /less/ access. The reverse, 'asMutableArg', is the unsafe
-- direction (it claims write access to something declared read-only); fine for a
-- handle whose object you own, used with care.
asConstArg :: Marshal e (Ptr a -> lo) lo -> Marshal e (PtrConst a -> lo) lo
asConstArg (Marshal m) = Marshal $ \e loC k -> m e (loC . PtrConst.unsafeFromPtr) k

-- | The reverse of 'asConstArg': fill a @Ptr a@ argument from a @PtrConst a@
-- marshaller (the unsafe direction; see 'asConstArg').
asMutableArg :: Marshal e (PtrConst a -> lo) lo -> Marshal e (Ptr a -> lo) lo
asMutableArg (Marshal m) = Marshal $ \e lo k -> m e (lo . PtrConst.unsafeToPtr) k

-- | Supply a fixed C argument the Haskell caller never sets (a NULL pointer, a
-- zero count, an @is_bare@ flag). This is 'scratch' specialised to a constant;
-- it adds no wrapper argument. See FINDINGS.md on the name.
fixed :: ThreadIn lo hi => c -> ToHighLevel lo hi -> ToHighLevel (c -> lo) hi
fixed c = scratch (\k -> k c)
