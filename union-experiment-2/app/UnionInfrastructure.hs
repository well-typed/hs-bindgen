-- | Union infrastructure
--
-- This would be part of hs-bindgen-runtime
module UnionInfrastructure (
    -- * 'StorableInContext'
    ReadRawWithCtxt(..)
  , readRawWithCtxtOff
    -- * 'StructField'
  , StructField(..)
  ) where

import Foreign
import GHC.TypeLits

{-------------------------------------------------------------------------------
  'ReadRawWithCtxt'

  One instance of this class would be derived by @hs-bindgen@ (where we are
  given the tag directly). Further instances can be used-defined.
-------------------------------------------------------------------------------}

class ReadRawWithCtxt ctxt a where
  readRawWithCtxt :: ctxt -> Ptr a -> IO a

readRawWithCtxtOff ::
     ReadRawWithCtxt ctxt a
  => ctxt -> Ptr b -> Int -> IO a
readRawWithCtxtOff ctxt addr off = readRawWithCtxt ctxt (addr `plusPtr` off)

{-------------------------------------------------------------------------------
  'StructField' allows to distinguish multiple unions in the (very rare?) case
  where the same union type occurs more than once in the same struct type.
-------------------------------------------------------------------------------}

newtype StructField (field :: Symbol) a = StructField a
