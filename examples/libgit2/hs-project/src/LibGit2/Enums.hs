{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- All instances here are orphans: the classes ('DefaultOut', 'DefaultIn') live in
-- hs-bindgen-runtime, and the types they mention (the generated enums, 'Oid',
-- 'Signature', 'Text') in other modules. Gathering them here keeps them out of the
-- hand-written wrappers; hs-bindgen could instead emit each generated type's
-- instance next to its declaration, where it would not be an orphan.
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Default marshaller instances for libgit2 types, so @output defaultOut@ and
-- @input defaultIn@ fill a position with no hand-written marshaller named at the
-- call site.
--
-- * 'DefaultOut' for each enum newtype. A libgit2 enum is a @newtype@ over a C
--   integer the generated function returns directly, so the instance is the
--   identity (@unmarshalOutPure id@). Enum /inputs/ need no instance here: the
--   runtime's general-identity 'DefaultIn' already passes an enum through (the
--   filemode in 'LibGit2.Write.treebuilderInsert').
--
-- * 'DefaultIn' for 'Text' (as @const char *@ and @char *@), 'Oid' (as
--   @const git_oid *@), and 'Signature' (as @const git_signature *@). Each
--   forwards to the marshaller "LibGit2.Marshal" \/ "LibGit2.Signature" already
--   defines, making the type a first-class @defaultIn@ \/ @auto@ input for callers.
--   The two 'Text' instances mirror the runtime's own @const@ \/ non-@const@
--   @String@ defaults. The wrappers here mostly still name their marshallers
--   explicitly (a handle argument has no 'DefaultIn', so @auto@ never fills a whole
--   call), so these serve callers building on the bindings more than the bindings
--   themselves.
module LibGit2.Enums () where

import Data.Text (Text)
import Foreign.C.Types (CChar)
import Foreign.Ptr (Ptr)

import HsBindgen.Runtime.PtrConst (PtrConst)

import HsBindgen.HighLevel.Defaults (DefaultIn (..), DefaultOut (..))
import HsBindgen.HighLevel.Marshaller (unmarshalOutPure)

import Generated.Oid (Git_oid)
import Generated.Types (Git_branch_t, Git_filemode_t, Git_object_t,
                        Git_reference_t, Git_signature, Git_submodule_ignore_t,
                        Git_submodule_recurse_t, Git_submodule_update_t)
import LibGit2.Marshal (asArgumentC, oidInC, textIn, textInPtr)
import LibGit2.Signature (sigMarshal)
import LibGit2.Types (Oid, Signature)

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

-- Input: forward each high-level type to the marshaller it already has, so
-- @input defaultIn@ fills the argument with no marshaller named at the call site.
instance DefaultIn Text (PtrConst CChar -> lo) lo where
  defaultIn = textIn
instance DefaultIn Text (Ptr CChar -> lo) lo where
  defaultIn = textInPtr
instance DefaultIn Oid (PtrConst Git_oid -> lo) lo where
  defaultIn = oidInC
instance DefaultIn Signature (PtrConst Git_signature -> lo) lo where
  defaultIn = asArgumentC sigMarshal
