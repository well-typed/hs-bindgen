-- | Fixtures and helpers shared across the @ToHighLevel@ test modules.
module Test.HsBindgen.HighLevel.Util (
    -- * Exception assertions
    assertThrows
    -- * Reusable outputs and C-callable stand-ins
  , peekIntOut
  , returnsStatus
  , cAdd
  , checkPresent
  , cBsLen
  , reportLen
  , firstByteConst
  , firstArgChar
    -- * libc-backed callables
  , c_strlen
  , c_strlen_raw
  , c_strncmp
    -- * A callback stand-in
  , Callback
  , callOnce
  ) where

import Control.Exception (Exception, try)
import Foreign.C.Types (CChar, CInt (..), CSize (..), CUChar, CULLong)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import GHC.Stack (HasCallStack)
import Test.Tasty.HUnit (Assertion, assertFailure)

import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import HsBindgen.HighLevel.Marshaller (Unmarshaller, unmarshalOutPure)

-- | Run an action that should throw @e@ and check the value thrown; fail the
-- test if it returns normally instead. One helper in place of the
-- @try@ \/ @case@ \/ @fail@ block the exception tests would otherwise repeat.
assertThrows :: (HasCallStack, Exception e) => IO a -> (e -> Assertion) -> Assertion
assertThrows act check = do
    r <- try act
    case r of
      Left e  -> check e
      Right _ -> assertFailure "expected an exception, but the action returned"

-- | Peek an @int@ out-parameter as an 'Int': the scalar output reused by the
-- output, closer, and error tests.
peekIntOut :: Unmarshaller (Ptr CInt) Int
peekIntOut = unmarshalOutPure (\(CInt n) -> fromIntegral n)

-- | A C call that returns its status argument unchanged, for exercising closers.
returnsStatus :: CInt -> IO CInt
returnsStatus = pure

-- | A trivial two-argument @int@ call, the stand-in for the @auto@, @fixed@, and
-- @assertPure@ tests.
cAdd :: CInt -> CInt -> IO CInt
cAdd a b = pure (a + b)

-- | Returns @1@ for a non-NULL @const char *@, @0@ for NULL.
checkPresent :: PtrConst CChar -> IO CInt
checkPresent p
  | PtrConst.unsafeToPtr p == nullPtr = pure 0
  | otherwise                         = pure 1

-- | Count the bytes of a @(const char *, size_t)@ pair.
cBsLen :: PtrConst CChar -> CSize -> IO CInt
cBsLen _ n = pure (fromIntegral n)

-- | Count the bytes of a @(const uchar *, unsigned long long)@ pair.
reportLen :: PtrConst CUChar -> CULLong -> IO CInt
reportLen _ n = pure (fromIntegral n)

-- | First byte of a @const char *@ argument.
firstByteConst :: PtrConst CChar -> IO CInt
firstByteConst p = fromIntegral <$> PtrConst.peek p

-- | First character of the first string of a @const char *const *@.
firstArgChar :: PtrConst (PtrConst CChar) -> IO CInt
firstArgChar pp = do
    p0 <- peek (castPtr (PtrConst.unsafeToPtr pp) :: Ptr (Ptr CChar))
    fromIntegral <$> peek p0

foreign import ccall unsafe "string.h strlen"
  c_strlen_raw :: Ptr CChar -> IO CSize

foreign import ccall unsafe "string.h strncmp"
  c_strncmp_raw :: Ptr CChar -> Ptr CChar -> CSize -> IO CInt

-- | @strlen@ with a @const@-correct pointer. The 'foreign import' uses
-- @Ptr CChar@ because marshalling @PtrConst CChar@ needs the @ConstPtr@ data
-- constructor, which only exists on @base@ 4.18+.
c_strlen :: PtrConst CChar -> IO CSize
c_strlen = c_strlen_raw . PtrConst.unsafeToPtr

-- | @strncmp@ with @const@-correct pointers.
c_strncmp :: PtrConst CChar -> PtrConst CChar -> CSize -> IO CInt
c_strncmp p1 p2 = c_strncmp_raw (PtrConst.unsafeToPtr p1) (PtrConst.unsafeToPtr p2)

type Callback = CInt -> IO ()

foreign import ccall "dynamic"
  mkCallback :: FunPtr Callback -> Callback

-- | Invoke a callback 'FunPtr' once, passing @42@.
callOnce :: FunPtr Callback -> IO ()
callOnce fp = mkCallback fp 42
