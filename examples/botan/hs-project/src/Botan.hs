module Botan (
    -- * Secure Remote Password
    ServerSession
  , withServerSession
  , srp6ServerSessionInit
  , srp6ServerSessionDestroy
  , Username (..)
  , Password (..)
  , Salt (..)
  , Verifier (..)
  , A (..)
  , B (..)
  , K (..)
  , srp6ServerSessionStep1
  , srp6ServerSessionStep2
  , srp6GenerateVerifier
  , srp6ClientAgree
  , srp6GroupSize
    -- * RNG
  , RNG
  , withRng
  , RNGType (..)
  , rngTypeString
  , rngInit
  , rngDestroy
  , rngGet
    -- * Discrete logarithm group
  , GroupId (..)
  , groupIdString
    -- * Hash functions
  , HashId (..)
  , hashIdString
  ) where


import Control.Exception (bracket)
import Control.Monad (void)
import Data.Vector.Storable qualified as VS
import Data.Word (Word8)
import Foreign.C.Error (throwErrnoIf)
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt, CSize)
import Foreign.Marshal (alloca, allocaBytes)
import Foreign.Storable (Storable (peek, poke))

import HsBindgen.Runtime.IncompleteArray qualified as IA
import HsBindgen.Runtime.Prelude
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import Generated.Botan (Botan_rng_t, Botan_srp6_server_session_t)
import Generated.Botan.Safe (botan_rng_destroy, botan_rng_get, botan_rng_init,
                             botan_srp6_client_agree,
                             botan_srp6_generate_verifier,
                             botan_srp6_group_size,
                             botan_srp6_server_session_destroy,
                             botan_srp6_server_session_init,
                             botan_srp6_server_session_step1,
                             botan_srp6_server_session_step2)

throwErrnoIfNegative :: String -> IO CInt -> IO ()
throwErrnoIfNegative label k = do
  ret <- k
  void $ throwErrnoIf (<0) (label ++ show (ret,ret)) (pure ret)

{-------------------------------------------------------------------------------
  Secure Remote Password
-------------------------------------------------------------------------------}

newtype ServerSession = ServerSession Botan_srp6_server_session_t

withServerSession :: (ServerSession -> IO a) -> IO a
withServerSession =
    bracket srp6ServerSessionInit srp6ServerSessionDestroy

srp6ServerSessionInit :: IO ServerSession
srp6ServerSessionInit =
    alloca $ \ptr -> do
      throwErrnoIfNegative "botan_srp6_server_session_init" $
        botan_srp6_server_session_init ptr
      ServerSession <$> peek ptr

srp6ServerSessionDestroy :: ServerSession -> IO ()
srp6ServerSessionDestroy (ServerSession s) =
    throwErrnoIfNegative "botan_srp6_server_session_destroy" $
      botan_srp6_server_session_destroy s

newtype Username = Username String
  deriving newtype (Show, Eq, Ord)

newtype Password = Password String
  deriving newtype (Show, Eq, Ord)

newtype Salt = Salt (IncompleteArray Word8)
  deriving newtype (Show, Eq)

newtype Verifier = Verifier (IncompleteArray Word8)
  deriving newtype (Show, Eq)

newtype A = A (IncompleteArray Word8)
  deriving newtype (Show, Eq)

newtype B = B (IncompleteArray Word8)
  deriving newtype (Show, Eq)

newtype K = K (IncompleteArray Word8)
  deriving newtype (Show, Eq)

srp6ServerSessionStep1 ::
     ServerSession
  -> Verifier
  -> GroupId
  -> HashId
  -> RNG
  -> IO B
srp6ServerSessionStep1 (ServerSession s) (Verifier verifier) groupId hashId (RNG rngObj) =
    IA.withPtr verifier $ \verifierPtr ->
    let verifierLen = fromIntegral $ VS.length $ IA.toVector verifier in
    withCString (groupIdString groupId) $ \groupIdPtr ->
    withCString (hashIdString hashId) $ \hashIdPtr ->
    srp6GroupSize groupId >>= \maxLen ->
    allocaBytes (fromIntegral maxLen) $ \bPtr ->
    alloca $ \bLenPtr -> do
      poke bLenPtr maxLen
      throwErrnoIfNegative "botan_srp6_server_session_step1" $
        botan_srp6_server_session_step1
          s
          (PtrConst.unsafeFromPtr verifierPtr)
          verifierLen
          (PtrConst.unsafeFromPtr groupIdPtr)
          (PtrConst.unsafeFromPtr hashIdPtr)
          rngObj
          bPtr
          bLenPtr
      bLen <- peek bLenPtr
      B <$> IA.peekArray (fromIntegral bLen) bPtr

srp6ServerSessionStep2 ::
     ServerSession
  -> GroupId
  -> A
  -> IO K
srp6ServerSessionStep2 (ServerSession s) groupId (A a) =
    IA.withPtr a $ \aPtr ->
    let aLen = fromIntegral $ VS.length $ IA.toVector a in
    srp6GroupSize groupId >>= \maxLen ->
    allocaBytes (fromIntegral maxLen) $ \kPtr ->
    alloca $ \kLenPtr -> do
      poke kLenPtr maxLen
      throwErrnoIfNegative "botan_srp6_server_session_step2" $
        botan_srp6_server_session_step2
          s
          (PtrConst.unsafeFromPtr aPtr)
          aLen
          kPtr
          kLenPtr
      kLen <- peek kLenPtr
      K <$> IA.peekArray (fromIntegral kLen) kPtr

srp6GenerateVerifier ::
     Username
  -> Password
  -> Salt
  -> GroupId
  -> HashId
  -> IO Verifier
srp6GenerateVerifier (Username user) (Password pw) (Salt salt) groupId hashId =
    withCString user $ \userPtr ->
    withCString pw $ \pwPtr ->
    IA.withPtr salt $ \saltPtr ->
    let saltLen = fromIntegral $ VS.length $ IA.toVector salt in
    withCString (groupIdString groupId) $ \groupIdPtr ->
    withCString (hashIdString hashId) $ \hashIdPtr ->
    srp6GroupSize groupId >>= \maxLen ->
    allocaBytes (fromIntegral maxLen) $ \verifierPtr ->
    alloca $ \verifierLenPtr -> do
      poke verifierLenPtr maxLen
      throwErrnoIfNegative "botan_srp6_generate_verifier" $
        botan_srp6_generate_verifier
          (PtrConst.unsafeFromPtr userPtr)
          (PtrConst.unsafeFromPtr pwPtr)
          (PtrConst.unsafeFromPtr saltPtr)
          saltLen
          (PtrConst.unsafeFromPtr groupIdPtr)
          (PtrConst.unsafeFromPtr hashIdPtr)
          verifierPtr
          verifierLenPtr
      verifierLen <- peek verifierLenPtr
      Verifier <$> IA.peekArray (fromIntegral verifierLen) verifierPtr

srp6ClientAgree ::
     Username
  -> Password
  -> GroupId
  -> HashId
  -> Salt
  -> B
  -> RNG
  -> IO (A, K)
srp6ClientAgree (Username user) (Password pw) groupId hashId (Salt salt) (B b) (RNG rngObj) =
    withCString user $ \userPtr ->
    withCString pw $ \pwPtr ->
    withCString (groupIdString groupId) $ \groupIdPtr ->
    withCString (hashIdString hashId) $ \hashIdPtr ->
    IA.withPtr salt $ \saltPtr ->
    let saltLen = fromIntegral $ VS.length $ IA.toVector salt in
    IA.withPtr b $ \bPtr ->
    let bLen = fromIntegral $ VS.length $ IA.toVector b in
    srp6GroupSize groupId >>= \maxLen ->
    allocaBytes (fromIntegral maxLen) $ \aPtr ->
    alloca $ \aLenPtr ->
    allocaBytes (fromIntegral maxLen) $ \kPtr ->
    alloca $ \kLenPtr -> do
      poke aLenPtr maxLen
      poke kLenPtr maxLen
      throwErrnoIfNegative "botan_srp6_client_agree" $
        botan_srp6_client_agree
          (PtrConst.unsafeFromPtr userPtr)
          (PtrConst.unsafeFromPtr pwPtr)
          (PtrConst.unsafeFromPtr groupIdPtr)
          (PtrConst.unsafeFromPtr hashIdPtr)
          (PtrConst.unsafeFromPtr saltPtr)
          saltLen
          (PtrConst.unsafeFromPtr bPtr)
          bLen
          rngObj
          aPtr
          aLenPtr
          kPtr
          kLenPtr
      aLen <- peek aLenPtr
      a <- A <$> IA.peekArray (fromIntegral aLen) aPtr
      kLen <- peek kLenPtr
      k <- K <$> IA.peekArray (fromIntegral kLen) kPtr
      pure (a, k)

srp6GroupSize :: GroupId -> IO CSize
srp6GroupSize groupId =
    withCString (groupIdString groupId) $ \groupIdPtr ->
    alloca $ \resPtr -> do
      throwErrnoIfNegative "botan_srp6_group_size" $
        botan_srp6_group_size (PtrConst.unsafeFromPtr groupIdPtr) resPtr
      peek resPtr

{-------------------------------------------------------------------------------
  RNG
-------------------------------------------------------------------------------}

newtype RNG = RNG Botan_rng_t

withRng :: RNGType -> (RNG -> IO a) -> IO a
withRng rngType = bracket (rngInit rngType) rngDestroy

data RNGType =
    System
  | User
  | UserThreadsafe
  | Null

rngTypeString :: RNGType -> String
rngTypeString = \case
    System -> "system"
    User -> "user"
    UserThreadsafe -> "user-threadsafe"
    Null -> "null"

rngInit :: RNGType -> IO RNG
rngInit rngType =
    withCString (rngTypeString rngType) $ \rngTypePtr ->
    alloca $ \ptr -> do
      throwErrnoIfNegative "botan_rng_init" $ botan_rng_init ptr (PtrConst.unsafeFromPtr rngTypePtr)
      RNG <$> peek ptr

rngDestroy :: RNG -> IO ()
rngDestroy (RNG rng) =
    throwErrnoIfNegative "botan_rng_destroy" $
      botan_rng_destroy rng

rngGet :: RNG -> CSize -> IO (IncompleteArray Word8)
rngGet (RNG rng) outLen = do
    allocaBytes (fromIntegral outLen) $ \outPtr -> do
      throwErrnoIfNegative "botan_rng_get" $ botan_rng_get rng outPtr outLen
      IA.peekArray (fromIntegral outLen) (IA.toPtr outPtr)

{-------------------------------------------------------------------------------
  Discrete logarithm group
-------------------------------------------------------------------------------}

data GroupId =
    MODP_SRP_1024
  | MODP_SRP_1536
  | MODP_SRP_2048
  | MODP_SRP_3072
  | MODP_SRP_4096
  | MODP_SRP_6144
  | MODP_SRP_8192

groupIdString :: GroupId -> String
groupIdString groupId = "modp/srp/" ++ case groupId of
    MODP_SRP_1024 -> "1024"
    MODP_SRP_1536 -> "1536"
    MODP_SRP_2048 -> "2048"
    MODP_SRP_3072 -> "3072"
    MODP_SRP_4096 -> "4096"
    MODP_SRP_6144 -> "6144"
    MODP_SRP_8192 -> "8192"

{-------------------------------------------------------------------------------
  Hash functions
-------------------------------------------------------------------------------}

data HashId =
    SHA_256
  | SHA_512

hashIdString :: HashId -> String
hashIdString SHA_256 = "SHA-256"
hashIdString SHA_512 = "SHA-512"
