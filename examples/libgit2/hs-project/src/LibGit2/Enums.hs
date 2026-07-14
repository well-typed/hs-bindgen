{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- These are orphan instances: 'DefaultOut' lives in hs-bindgen-runtime, the enum
-- types in "Generated.Types". Gathering them in one module keeps them out of the
-- hand-written wrappers; hs-bindgen could instead emit each one next to its enum,
-- where it would not be an orphan.
{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'DefaultOut' instances for libgit2's enum newtypes, so @output defaultOut@ fills
-- an @enum *out@ position with no hand-written marshaller.
--
-- A libgit2 enum is a @newtype@ over a C integer the generated function takes and
-- returns directly, so the output instance is the identity (@unmarshalOutPure id@).
-- The /input/ direction needs nothing here: the runtime's general-identity
-- 'HsBindgen.Runtime.HighLevel.Defaults.DefaultIn' already passes an enum through, so
-- @input defaultIn@ (the filemode in 'LibGit2.Write.treebuilderInsert') resolves with
-- no instance of ours.
module LibGit2.Enums () where

import Foreign.Ptr (Ptr)

import HsBindgen.Runtime.HighLevel.Defaults (DefaultOut (..))
import HsBindgen.Runtime.HighLevel.Marshaller (unmarshalOutPure)

import Generated.Types (Git_branch_t, Git_filemode_t, Git_object_t,
                        Git_reference_t, Git_submodule_ignore_t,
                        Git_submodule_recurse_t, Git_submodule_update_t)

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
