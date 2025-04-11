-- | Union infrastructure
--
-- This would be part of hs-bindgen-runtime
module UnionInfrastructure (
    -- * 'StorableInContext'
    StorableInContext(..)
  , pokeByteOffInCtxt
  , peekByteOffInCtxt
    -- * 'StructHasUnionTag'
  , StructHasUnionTag(..)
  ) where

import Data.Kind
import Foreign
import GHC.TypeLits
import Foreign.C
import Data.Proxy

{-------------------------------------------------------------------------------
  'StorableInContext'

  Instances of this class would be derived by hs-bindgen.
-------------------------------------------------------------------------------}

class StorableInContext (ctxt :: Type) a where
  pokeInCtxt :: proxy ctxt -> Ptr a -> a -> IO ()
  peekInCtxt ::       ctxt -> Ptr a      -> IO a

pokeByteOffInCtxt ::
     StorableInContext ctxt a
  => proxy ctxt -> Ptr b -> Int -> a -> IO ()
pokeByteOffInCtxt ctxt addr off = pokeInCtxt ctxt (addr `plusPtr` off)

peekByteOffInCtxt ::
     StorableInContext ctxt a
  => ctxt -> Ptr b -> Int -> IO a
peekByteOffInCtxt ctxt addr off = peekInCtxt ctxt (addr `plusPtr` off)

{-------------------------------------------------------------------------------
  Used when deriving 'Storable' instances for structs containing unions

  Instances of this class would be hand-written by users of hs-bindgen.

  The general infrastructure above makes no assumptions about what the context
  is; 'StructHasUnionTag' is used in the /instance/ we generate. Note that the
  only thing struct-specific about 'StructHasUnionTag' is that we pass the field
  name of the union; this can be used to disambiguate multiple unions nested
  inside a struct.
-------------------------------------------------------------------------------}

class StructHasUnionTag ctxt (field :: Symbol) where
  structUnionTag :: Proxy field -> ctxt -> CUInt

