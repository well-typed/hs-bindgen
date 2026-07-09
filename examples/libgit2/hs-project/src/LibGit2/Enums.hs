{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- These are orphan instances: the 'DefaultIn' \/ 'DefaultOut' classes live in
-- hs-bindgen-runtime, the enum types in "Generated.Types". Gathering them in one
-- module keeps them out of the hand-written wrappers; hs-bindgen could instead
-- emit each one next to its enum, where it would not be an orphan.
{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'DefaultIn' \/ 'DefaultOut' instances for libgit2's enum newtypes, so @auto@
-- and @input defaultIn@ \/ @output defaultOut@ can fill an enum position with no
-- hand-written marshaller.
--
-- A libgit2 enum is a @newtype@ over a C integer that the generated function
-- takes and returns directly (it derives @HasFFIType@), so every instance is the
-- identity: @scalar id@ going in, @unmarshalOutPure id@ coming out. They are one
-- line each and always the same shape. That is the point of listing them here:
-- hs-bindgen already knows a declaration is an enum, so it has the information to
-- generate these. (@DefaultRes@, for enum-returning functions like
-- @git_object_type@, is the natural third and follows the same identity pattern.)
--
-- The @git_filemode_t@ instance is exercised by 'LibGit2.Write.treebuilderInsert',
-- which fills its filemode argument with @input defaultIn@.
module LibGit2.Enums () where

import Foreign.Ptr (Ptr)

import HsBindgen.Runtime.HighLevel.Defaults (DefaultIn (..), DefaultOut (..))
import HsBindgen.Runtime.HighLevel.Marshaller (scalar, unmarshalOutPure)

import Generated.Types (Git_branch_t, Git_filemode_t, Git_object_t,
                        Git_reference_t, Git_submodule_ignore_t,
                        Git_submodule_recurse_t, Git_submodule_update_t)

-- Input: the C argument is the enum newtype, so pass it through unchanged.
instance DefaultIn Git_object_t where
  type DefInArrow Git_object_t lo = Git_object_t -> lo
  defaultIn = scalar id
instance DefaultIn Git_reference_t where
  type DefInArrow Git_reference_t lo = Git_reference_t -> lo
  defaultIn = scalar id
instance DefaultIn Git_filemode_t where
  type DefInArrow Git_filemode_t lo = Git_filemode_t -> lo
  defaultIn = scalar id
instance DefaultIn Git_branch_t where
  type DefInArrow Git_branch_t lo = Git_branch_t -> lo
  defaultIn = scalar id
instance DefaultIn Git_submodule_ignore_t where
  type DefInArrow Git_submodule_ignore_t lo = Git_submodule_ignore_t -> lo
  defaultIn = scalar id
instance DefaultIn Git_submodule_recurse_t where
  type DefInArrow Git_submodule_recurse_t lo = Git_submodule_recurse_t -> lo
  defaultIn = scalar id
instance DefaultIn Git_submodule_update_t where
  type DefInArrow Git_submodule_update_t lo = Git_submodule_update_t -> lo
  defaultIn = scalar id

-- Output: peek the enum newtype out of an @enum *out@ parameter unchanged.
instance DefaultOut Git_object_t (Ptr Git_object_t) where
  defaultOut = unmarshalOutPure id
instance DefaultOut Git_reference_t (Ptr Git_reference_t) where
  defaultOut = unmarshalOutPure id
instance DefaultOut Git_filemode_t (Ptr Git_filemode_t) where
  defaultOut = unmarshalOutPure id
instance DefaultOut Git_branch_t (Ptr Git_branch_t) where
  defaultOut = unmarshalOutPure id
instance DefaultOut Git_submodule_ignore_t (Ptr Git_submodule_ignore_t) where
  defaultOut = unmarshalOutPure id
instance DefaultOut Git_submodule_recurse_t (Ptr Git_submodule_recurse_t) where
  defaultOut = unmarshalOutPure id
instance DefaultOut Git_submodule_update_t (Ptr Git_submodule_update_t) where
  defaultOut = unmarshalOutPure id
