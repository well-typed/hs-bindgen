-- | This is hs-bindgen specific debug tracing functionality.
-- Meant to be more useful than @liftIO $ print (foo, bar)@
module HsBindgen.Debug (
    dtraceIO,
    Repr,
    ReprShow (..),
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Array.Byte (ByteArray (..))
import Data.Bits (unsafeShiftR, (.&.))
import Data.Char (intToDigit)
import Data.Text qualified as T
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import GHC.Exts qualified
import GHC.Generics (from, (:*:) (..), M1 (..), K1 (..))

import Clang.Enum.Simple (fromSimpleEnum)
import Clang.Internal.ByValue (OnHaskellHeap (..))
import Clang.LowLevel.Core (CXType (..), CXCursor (..), CXSourceLocation, clang_getPresumedLocation, cxtKind, clang_getCursorKind)

dtraceIO :: (MonadIO m, Repr a) => String -> a -> m ()
dtraceIO tag xs = liftIO $ do
    xs' <- repr xs
    putStrLn $ tag ++ ": " ++ unwords (xs' [])

-- | Debug representation of a value.
class Repr a where
    -- | may do IO, returns difference list
    repr :: a -> IO ([String] -> [String])

    default repr :: Show a => a -> IO ([String] -> [String])
    repr x = repr (ReprShow x)

-- | 'ReprShow' is usable for @DerivingVia@, but also to 'repr' things which don't have 'Repr' instance, but have 'Show' instance:
--
-- @
-- dtraceIO (cursor, ReprShow something)
-- @
--
newtype ReprShow a = ReprShow a
  deriving newtype Show

instance Show a => Repr (ReprShow a) where
    repr (ReprShow x) = return (\xs -> showsPrec 11 x "" : xs)

-------------------------------------------------------------------------------
-- Repr tuples
-------------------------------------------------------------------------------

instance (Repr t1, Repr t2) => Repr (t1, t2) where repr = genericProductRepr . from
instance (Repr t1, Repr t2, Repr t3) => Repr (t1, t2, t3) where repr = genericProductRepr . from
instance (Repr t1, Repr t2, Repr t3, Repr t4) => Repr (t1, t2, t3, t4) where repr = genericProductRepr . from
instance (Repr t1, Repr t2, Repr t3, Repr t4, Repr t5) => Repr (t1, t2, t3, t4, t5) where repr = genericProductRepr . from
instance (Repr t1, Repr t2, Repr t3, Repr t4, Repr t5, Repr t6) => Repr (t1, t2, t3, t4, t5, t6) where repr = genericProductRepr . from

-- | Generic @repr@ derivation for products, to help define tuple instances
class GProductRepr f where genericProductRepr :: f a -> IO ([String] -> [String])

instance GProductRepr f => GProductRepr (M1 i c f) where genericProductRepr (M1 f) = genericProductRepr f
instance Repr a => GProductRepr (K1 r a) where genericProductRepr (K1 x) = repr x

instance (GProductRepr f, GProductRepr g) => GProductRepr (f :*: g) where
    genericProductRepr (f :*: g) = do
        f' <- genericProductRepr f
        g' <- genericProductRepr g
        return (f' . g')

-------------------------------------------------------------------------------
-- Repr basic types
-------------------------------------------------------------------------------

deriving via ReprShow T.Text instance Repr T.Text
deriving via ReprShow Bool instance Repr Bool
deriving via ReprShow Int instance Repr Int
deriving via ReprShow () instance Repr ()
deriving via ReprShow CInt instance Repr CInt

instance Repr a => Repr [a] where
    repr xs = do
        ys <- traverse repr xs
        let ys' :: String
            ys' = '[' : unwords (foldr id [] ys) ++ "]"
        return $ \zs -> ys' : zs

-------------------------------------------------------------------------------
-- Repr libclang
-------------------------------------------------------------------------------

reprOnHaskellHeap :: Int -> OnHaskellHeap tag -> String
reprOnHaskellHeap pfx (OnHaskellHeap ba) = go 1 (drop pfx $ GHC.Exts.toList $ ByteArray ba) [] where
    go :: Int -> [Word8] -> ShowS
    go !_ [] = id
    go  n (x:xs) = showsWord8 x . separator (null xs) n . go (n + 1) xs

    separator :: Bool -> Int -> ShowS
    separator True _ = id
    separator False n | n `mod` 4 == 0 = showChar '_'
                      | otherwise      = id

showsWord8 :: Word8 -> ShowS
showsWord8 !w s = intToDigit (fromIntegral (unsafeShiftR w 4)) : intToDigit (fromIntegral (w .&. 0x0f)) : s

-- | We drop first 4 bytes of enum CXCursorKind kind;
--
-- @
-- typedef struct {
--   enum CXCursorKind kind;
--   int xdata;
--   const void *data[3];
-- } CXCursor;
-- @
--
instance Repr CXCursor where
    repr cursor@(CXCursor bytes) = do
        k <- clang_getCursorKind cursor
        let !r = case fromSimpleEnum k of
                Left i  -> "CXCursor(" ++ show i ++")"
                Right k' -> show k'
        return (\xs -> (r ++ "#" ++ reprOnHaskellHeap 4 bytes) : xs)

-- | We drop first 8 bytes (enum kind and padding)
--
-- @
-- typedef struct {
--   enum CXTypeKind kind;
--   void *data[2];
-- } CXType;
-- @
instance Repr CXType where
    repr ty@(CXType bytes) = do
        let !r = case fromSimpleEnum (cxtKind ty) of
                Left i  -> "CXType(" ++ show i ++")"
                Right k' -> show k'
        return (\xs -> (r ++ "#" ++ reprOnHaskellHeap 8 bytes) : xs)

instance Repr CXSourceLocation where
    repr loc = do
        (path, col, row) <- clang_getPresumedLocation loc
        let r = shows path . showChar ':' . shows col . showChar ':' . shows row
        return (\xs -> r "" : xs)
