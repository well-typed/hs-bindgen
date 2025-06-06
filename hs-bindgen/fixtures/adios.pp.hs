{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (Bounded, Enum, Eq, IO, Integral, Num, Ord, Read, Real, Show)

$(CAPI.addCSource "#include \"adios.h\"\nvoid testmodule_\978 (void) { \978(); }\nvoid testmodule_\25308\25308 (void) { \25308\25308(); }\nvoid testmodule_Say\25308\25308 (void) { Say\25308\25308(); }\n")

newtype Adio'0301s = Adio'0301s
  { un_Adio'0301s :: FC.CInt
  }

deriving newtype instance F.Storable Adio'0301s

deriving stock instance Eq Adio'0301s

deriving stock instance Ord Adio'0301s

deriving stock instance Read Adio'0301s

deriving stock instance Show Adio'0301s

deriving newtype instance Enum Adio'0301s

deriving newtype instance Ix.Ix Adio'0301s

deriving newtype instance Bounded Adio'0301s

deriving newtype instance Bits.Bits Adio'0301s

deriving newtype instance FiniteBits Adio'0301s

deriving newtype instance Integral Adio'0301s

deriving newtype instance Num Adio'0301s

deriving newtype instance Real Adio'0301s

newtype C数字 = C数字
  { un_C数字 :: FC.CInt
  }

deriving newtype instance F.Storable C数字

deriving stock instance Eq C数字

deriving stock instance Ord C数字

deriving stock instance Read C数字

deriving stock instance Show C数字

deriving newtype instance Enum C数字

deriving newtype instance Ix.Ix C数字

deriving newtype instance Bounded C数字

deriving newtype instance Bits.Bits C数字

deriving newtype instance FiniteBits C数字

deriving newtype instance Integral C数字

deriving newtype instance Num C数字

deriving newtype instance Real C数字

foreign import ccall safe "testmodule_ϒ" cϒ :: IO ()

foreign import ccall safe "testmodule_拜拜" 拜拜 :: IO ()

foreign import ccall safe "testmodule_Say拜拜" say拜拜 :: IO ()
