{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Minisat.Generated where

import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

import Data.Bits (FiniteBits)
import Data.Bits qualified as Bits
import Data.Ix qualified as Ix
import Foreign qualified as F
import Foreign.C qualified as FC

{-| __C declaration:__ @minisat_solver@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:26:16@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
data Minisat_solver

{-| __C declaration:__ @opaque@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:30:9@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
opaque :: forall a0. a0 -> a0
opaque = \x0 -> x0

{-| __C declaration:__ @minisat_Var@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:32:21@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
newtype Minisat_Var = Minisat_Var
  { un_Minisat_Var :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @minisat_Lit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:33:21@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
newtype Minisat_Lit = Minisat_Lit
  { un_Minisat_Lit :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @minisat_lbool@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:34:21@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
newtype Minisat_lbool = Minisat_lbool
  { un_Minisat_lbool :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @minisat_bool@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:35:21@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
newtype Minisat_bool = Minisat_bool
  { un_Minisat_bool :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)
