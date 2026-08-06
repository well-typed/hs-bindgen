-- 'newHandle' names its spec's output list ('[h]) in a type signature, which needs
-- DataKinds. Any binding that writes a ToHighLevel type of its own does.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Reusable marshallers for the shapes libgit2 uses everywhere: managed-handle
-- out-parameters, handles as @T *@ \/ @const T *@ arguments, by-value @git_oid@ in
-- and out, and borrowed C strings.
--
-- 'newHandle' covers every libgit2 constructor and the @borrowed*@ family covers
-- every accessor, so most modules here name one of the two and supply its inputs.
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
  , peekTextConst
  , peekText
    -- * Borrowed-pointer accessors
  , borrowedText
  , borrowedOid
  , borrowedScalar
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Foreign.C.String (peekCString)
import Foreign.C.Types (CChar, CInt)
import Foreign.ForeignPtr (FinalizerPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)

import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import HsBindgen.HighLevel (ToHighLevel, input, output, resultIO, resultPure,
                            toHighLevel)
import HsBindgen.HighLevel.Internal.Threading (ThreadIn)
import HsBindgen.HighLevel.Marshaller (Marshal, Unmarshaller, asConstArg, at,
                                       bracket, unmarshalOut)
import HsBindgen.HighLevel.Marshaller.Utils (outForeignPtr, withCStringIn,
                                             withCStringMutIn)

import Generated.Oid (Git_oid)
import LibGit2.Error (checkedStatus)
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
-- @git_X_free@ finaliser, so the handle frees itself at GC. The runtime's
-- 'outForeignPtr' does the allocate-peek-wrap; this only maps the raw 'ForeignPtr'
-- into the binding's handle @newtype@ (via 'fromFP').
outHandle
  :: forall h. Handle h
  => FinalizerPtr (CRep h)
  -> Unmarshaller (Ptr (Ptr (CRep h))) h
outHandle = fmap fromFP . outForeignPtr

-- | Build a managed-handle constructor: the shape every libgit2 @git_X_new@ \/
-- @git_X_lookup@ \/ @git_X_open@ shares. @newHandle fin inputs cfn@ fills the
-- @git_X **out@ slot (freed by @fin@), applies the caller's @inputs@ chain, and
-- throws unless the libgit2 status says the handle is real:
--
-- > repositoryOpen :: Text -> IO Repository
-- > repositoryOpen = newHandle git_repository_free (input textIn) git_repository_open
--
-- The @inputs@ argument is the caller's own slice of the spec, still open at the
-- bottom: it takes the closer and returns the chain with that closer attached, so
-- a caller writes @input textIn@ or @input handleIn . input oidInC@ and this
-- supplies both ends. The @'[h]@ in its type is the handle this constructor is
-- about to produce, waiting in the spec's output list for the closer to collect.
newHandle
  :: (Handle h, ThreadIn hi)
  => FinalizerPtr (CRep h)
  -> (ToHighLevel '[h] (IO CInt) (IO h) -> ToHighLevel '[h] lo hi)
  -> (Ptr (Ptr (CRep h)) -> lo)
  -> hi
newHandle fin inputs = flip toHighLevel
                     $ output (outHandle fin)
                     $ inputs checkedStatus
-- Without this, the higher-order @inputs@ argument puts 'newHandle' over GHC's
-- inlining threshold and every construction allocates the intermediate pair, the
-- deferred read-back closure and an 'Outputs' cell that would otherwise fuse away.
-- The other helpers here are small enough to inline without being told.
{-# INLINE newHandle #-}

{-------------------------------------------------------------------------------
  Object ids
-------------------------------------------------------------------------------}

-- | Marshal an 'Oid' as a non-@const@ @git_oid *@ (pointer to a temporary copy).
oidIn :: Marshal Oid (Ptr Git_oid -> lo) lo
oidIn = bracket (\(Oid g) k -> with g k)

-- | Marshal an 'Oid' as a @const git_oid *@ (the @const@ form of 'oidIn').
oidInC :: Marshal Oid (PtrConst Git_oid -> lo) lo
oidInC = asConstArg oidIn

-- | An out-parameter @git_oid *@ that C fills.
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
-- @git_signature@ name/email field), the 'Text' adapter over the runtime's
-- 'withCStringMutIn' (which the runtime keeps @String@-based to stay @text@-free).
textInPtr :: Marshal Text (Ptr CChar -> lo) lo
textInPtr = at T.unpack withCStringMutIn

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
borrowedText
  :: Handle h
  => (PtrConst (CRep h) -> IO (PtrConst CChar))
  -> (h -> IO Text)
borrowedText = flip toHighLevel
             $ input handleInC
             $ resultIO peekTextConst

-- | A @const T *@ accessor returning a borrowed @const git_oid *@, copied to 'Oid'.
borrowedOid
  :: Handle h
  => (PtrConst (CRep h) -> IO (PtrConst Git_oid))
  -> (h -> IO Oid)
borrowedOid = flip toHighLevel
            $ input handleInC
            $ resultIO peekOidConst

-- | A @const T *@ accessor returning a C scalar, converted with 'fromIntegral'.
borrowedScalar
  :: (Handle h, Integral c, Num n)
  => (PtrConst (CRep h) -> IO c)
  -> (h -> IO n)
borrowedScalar = flip toHighLevel
               $ input handleInC
               $ resultPure fromIntegral
