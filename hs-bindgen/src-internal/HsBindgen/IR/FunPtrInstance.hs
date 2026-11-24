-- | The @ToFunPtr@ and @FromFunPtr@ instances to generate
--
-- Intended for unqualified import.
module HsBindgen.IR.FunPtrInstance (
    FunPtrInstance(..)
  , FunPtrHead(..)
  , FunPtrArg(..)
  , FunPtrRes(..)
  ) where

import HsBindgen.Frontend.Pass.MangleNames.IsPass (NewtypeNames)
import HsBindgen.IR.FunSig
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.Language.Haskell qualified as Hs

-- | A pair of @ToFunPtr@ and @FromFunPtr@ instances to generate, with their
-- @"wrapper"@ and @"dynamic"@ imports
--
-- All arguments and the result are passed directly: the imports pass FFI
-- types only.
data FunPtrInstance = FunPtrInstance {
      head :: FunPtrHead
    , sig  :: FunSig FunPtrArg FunPtrRes
    }
  deriving stock (Show, Eq)

-- | What the instances are for
data FunPtrHead =
    -- | The function type itself: @instance ToFunPtr (T -> IO ())@
    HeadFunType
    -- | The newtype of a typedef of a function type (@typedef void F(T)@), or
    -- the @_Aux@ newtype of a typedef of a pointer to one
    -- (@typedef void (*F)(T)@): @instance ToFunPtr F@
  | HeadNewtype Hs.Identifier NewtypeNames
  deriving stock (Show, Eq, Ord)

-- | An argument of the function type
data FunPtrArg = FunPtrArg {
      -- | Translated in context @FunArg@
      hs  :: Hs.Type
    , ffi :: Hs.FFIType
    }
  deriving stock (Show, Eq)

-- | The result of the function type
data FunPtrRes = FunPtrRes {
      -- | Translated in context @FunRes@
      hs  :: Hs.Type
      -- | Always in 'IO': 'Hs.FFIResIOUnit' or 'Hs.FFIResIO'
    , ffi :: Hs.FFIResType
    }
  deriving stock (Show, Eq)
