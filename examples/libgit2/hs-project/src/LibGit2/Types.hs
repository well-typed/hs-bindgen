{-# LANGUAGE TypeFamilies #-}

-- | High-level value and handle types for the libgit2 bindings.
--
-- Opaque libgit2 objects (@git_repository@, @git_commit@, ...) are exposed as
-- newtypes over a 'ForeignPtr', so their @git_*_free@ finaliser runs at GC time
-- and the user never frees anything by hand. The 'Handle' class collapses the
-- per-type @withForeignPtr@ boilerplate into one method, which the marshallers
-- in "LibGit2.Marshal" build on.
--
module LibGit2.Types
  ( -- * Managed handles
    Repository (..)
  , Object (..)
  , Commit (..)
  , Tree (..)
  , Blob (..)
  , Reference (..)
  , Revwalk (..)
  , Index (..)
  , TreeBuilder (..)
  , Config (..)
  , Handle (..)
  , withHandle
    -- * Object ids
  , Oid (..)
  , oidToHex
  , oidToHexShort
    -- * Signatures
  , Signature (..)
  , GitTime (..)
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)

import HsBindgen.Runtime.ConstantArray qualified as CA

import Generated.Oid (Git_oid (..))
import Generated.Types qualified as G
import Numeric (showHex)

{-------------------------------------------------------------------------------
  Managed handles
-------------------------------------------------------------------------------}

newtype Repository  = Repository  (ForeignPtr G.Git_repository)
newtype Object      = Object      (ForeignPtr G.Git_object)
newtype Commit      = Commit      (ForeignPtr G.Git_commit)
newtype Tree        = Tree        (ForeignPtr G.Git_tree)
newtype Blob        = Blob        (ForeignPtr G.Git_blob)
newtype Reference   = Reference   (ForeignPtr G.Git_reference)
newtype Revwalk     = Revwalk     (ForeignPtr G.Git_revwalk)
newtype Index       = Index       (ForeignPtr G.Git_index)
newtype TreeBuilder = TreeBuilder (ForeignPtr G.Git_treebuilder)
newtype Config      = Config      (ForeignPtr G.Git_config)

-- | A high-level handle backed by a 'ForeignPtr' to an opaque libgit2 object.
-- One method pair replaces a @withForeignPtr@ / @newForeignPtr@ wrapper for
-- every one of the ten handle types above.
class Handle h where
  type CRep h
  toFP   :: h -> ForeignPtr (CRep h)
  fromFP :: ForeignPtr (CRep h) -> h

instance Handle Repository  where { type CRep Repository  = G.Git_repository;  toFP (Repository p)  = p; fromFP = Repository  }
instance Handle Object      where { type CRep Object      = G.Git_object;      toFP (Object p)      = p; fromFP = Object      }
instance Handle Commit      where { type CRep Commit      = G.Git_commit;      toFP (Commit p)      = p; fromFP = Commit      }
instance Handle Tree        where { type CRep Tree        = G.Git_tree;        toFP (Tree p)        = p; fromFP = Tree        }
instance Handle Blob        where { type CRep Blob        = G.Git_blob;        toFP (Blob p)        = p; fromFP = Blob        }
instance Handle Reference   where { type CRep Reference   = G.Git_reference;   toFP (Reference p)   = p; fromFP = Reference   }
instance Handle Revwalk     where { type CRep Revwalk     = G.Git_revwalk;     toFP (Revwalk p)     = p; fromFP = Revwalk     }
instance Handle Index       where { type CRep Index       = G.Git_index;       toFP (Index p)       = p; fromFP = Index       }
instance Handle TreeBuilder where { type CRep TreeBuilder = G.Git_treebuilder; toFP (TreeBuilder p) = p; fromFP = TreeBuilder }
instance Handle Config      where { type CRep Config      = G.Git_config;      toFP (Config p)      = p; fromFP = Config      }

-- | Run an action with the raw pointer of a handle, keeping the handle alive
-- for the duration (so the @git_*_free@ finaliser cannot fire mid-call).
withHandle :: Handle h => h -> (Ptr (CRep h) -> IO r) -> IO r
withHandle h = withForeignPtr (toFP h)

{-------------------------------------------------------------------------------
  Object ids
-------------------------------------------------------------------------------}

-- | A git object id (SHA-1), wrapping the generated 20-byte struct so it can be
-- marshalled in and out with the struct's own 'Foreign.Storable.Storable'
-- instance.
newtype Oid = Oid Git_oid
  deriving newtype (Eq)

instance Show Oid where
  show = T.unpack . oidToHex

-- | Full 40-character hex rendering, computed purely from the raw bytes (no FFI
-- call into @git_oid_tostr@).
oidToHex :: Oid -> Text
oidToHex (Oid g) =
    T.pack $ concatMap (hexByte . fromIntegral) (CA.toList (git_oid_id g))
  where
    hexByte :: Word8 -> String
    hexByte w =
      let s = showHex (fromIntegral w :: Word8) ""
       in if length s == 1 then '0' : s else s

-- | Abbreviated hex, @n@ characters (like @git log --abbrev@).
oidToHexShort :: Int -> Oid -> Text
oidToHexShort n = T.take n . oidToHex

{-------------------------------------------------------------------------------
  Signatures
-------------------------------------------------------------------------------}

-- | An author/committer signature, with the name and email copied out of C into
-- 'Text' (so it outlives the owning commit handle).
data Signature = Signature
  { sigName  :: Text
  , sigEmail :: Text
  , sigWhen  :: GitTime
  }
  deriving (Eq, Show)

-- | A timestamp: seconds since the Unix epoch plus a timezone offset in minutes.
data GitTime = GitTime
  { gitTimeEpoch     :: Int64
  , gitTimeOffsetMin :: Int
  }
  deriving (Eq, Show)
