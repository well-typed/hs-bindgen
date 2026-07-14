{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Reusable marshallers that plug into the @ToHighLevel@ combinators for the
-- shapes libgit2 uses everywhere: managed-handle out-parameters, handles as
-- @T *@ / @const T *@ arguments, by-value @git_oid@ in and out, borrowed C
-- strings, byte buffers, and structs passed as @const@ pointers.
--
-- 'newHandle' captures the one shape every constructor shares: a @git_X **out@
-- slot freed by the handle's @git_X_free@, the caller's inputs, and the libgit2
-- status check.
--
module LibGit2.Marshal
  ( -- * Handles
    handleIn
  , handleInC
  , outHandle
  , newHandle
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
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as BSU
import Data.Text (Text)
import Data.Text qualified as T
import Foreign.C.String (peekCString, withCString)
import Foreign.C.Types (CChar, CInt, CSize)
import Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)

import HsBindgen.Runtime.HighLevel (ToHighLevel, asArgument, input, output,
                                    resultIO, resultPure, toHighLevel)
import HsBindgen.Runtime.HighLevel.Internal.Threading (ThreadOut)
import HsBindgen.Runtime.HighLevel.Marshaller (Marshal (..), MarshalStruct,
                                               Unmarshaller, asConstArg, at,
                                               bracket, unmarshalOut,
                                               unmarshalOutWith)
import HsBindgen.Runtime.HighLevel.Marshaller.Utils (withCStringIn)
import HsBindgen.Runtime.Marshal (StaticSize, WriteRaw)
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import Generated.Oid (Git_oid)
import LibGit2.Error (checkStatusResult)
import LibGit2.Types (Handle (..), Oid (..), withHandle)

{-------------------------------------------------------------------------------
  Handles
-------------------------------------------------------------------------------}

-- | Pass a handle as a non-@const@ @T *@ argument. The handle stays alive across
-- the call.
handleIn :: Handle h => Marshal h (Ptr (CRep h) -> lo) lo
handleIn = bracket withHandle

-- | Pass a handle as a @const T *@ argument. This is 'handleIn' with its pointer
-- retagged @const@ by the runtime's 'asConstArg': one marshaller, two C argument
-- shapes.
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

-- | Build a managed-handle constructor. @newHandle fin inputs cfn@ fills the
-- @git_X **out@ slot (freed by @fin@), applies the caller's @inputs@ chain, and
-- checks the libgit2 status. The result carries a trailing @()@ from the status
-- check, dropped at the call site with @fst@:
--
-- > repositoryOpen path =
-- >   fst <$> newHandle git_repository_free (input textIn) git_repository_open path
newHandle
  :: (Handle h, ThreadOut (Ptr (Ptr (CRep h))) h hi hi')
  => FinalizerPtr (CRep h)
  -> (ToHighLevel (IO CInt) (IO ()) -> ToHighLevel lo hi)
  -> (Ptr (Ptr (CRep h)) -> lo)
  -> hi'
newHandle fin inputs cfn =
  toHighLevel (output (outHandle fin) (inputs checkStatusResult)) cfn

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

-- | Marshal a 'ByteString' as a @(const void *, size_t)@ pair (the pointee is
-- left polymorphic so it unifies with @void@ at the call site). The zero-copy
-- counterpart of the runtime's @constByteStringLenIn@: 'BSU.unsafeUseAsCStringLen'
-- passes the buffer without copying, so it is not NUL-terminated and is valid
-- only during the call and only alongside the length (an empty input may alias a
-- shared buffer). Safe here because the callee honours the length and does not
-- retain the pointer.
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

  Take a handle as @const T *@, call an accessor that returns a borrowed pointer,
  and copy it out.
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

-- | Drop a 'MarshalStruct' into a @const T *@ argument position: the runtime's
-- 'asArgument' fills a non-@const@ @Ptr@, then 'asConstArg' retags it @const@.
asArgumentC
  :: (StaticSize s, WriteRaw s)
  => MarshalStruct hi s
  -> Marshal hi (PtrConst s -> lo) lo
asArgumentC = asConstArg . asArgument

-- | A NULL @const@ pointer, for optional/absent C arguments.
nullConst :: PtrConst a
nullConst = PtrConst.unsafeFromPtr nullPtr
