{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Every instance here is an orphan: the 'DefaultIn' class lives in
-- hs-bindgen-highlevel, and the types it mentions ('Text', 'Oid', 'Signature') in
-- other modules. Gathering them here keeps them out of the hand-written wrappers;
-- hs-bindgen could instead emit each generated type's instance next to its
-- declaration, where it would not be an orphan.
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Default marshallers for the libgit2 value types, so that @input defaultIn@ (and
-- so @auto@) fills an argument with no marshaller named at the call site.
--
-- Each instance forwards to the marshaller "LibGit2.Marshal" \/ "LibGit2.Signature"
-- already defines: 'Text' as @const char *@ and @char *@, 'Oid' as
-- @const git_oid *@, 'Signature' as @const git_signature *@. One type can serve
-- several C shapes, which is why 'Text' appears twice, keyed on the pointer
-- constness the C function asks for.
--
-- These serve callers building on the binding more than the binding itself: a
-- libgit2 call almost always takes a handle, handles have no default, and so @auto@
-- rarely fills a whole call here.
--
-- The generated enums need no instances at all. They are newtypes over C integers
-- that pass through unchanged, and the library's identity defaults
-- (@DefaultIn a (a -> lo) lo@ and @Storable a => DefaultOut a (Ptr a)@) already
-- cover every one of them, in or out. See the filemode in
-- 'LibGit2.Write.treebuilderInsert'.
module LibGit2.Defaults () where

import Data.Text (Text)
import Foreign.C.Types (CChar)
import Foreign.Ptr (Ptr)

import HsBindgen.Runtime.PtrConst (PtrConst)

import HsBindgen.HighLevel (asArgumentC)
import HsBindgen.HighLevel.Defaults (DefaultIn (..))

import Generated.Oid (Git_oid)
import Generated.Types (Git_signature)
import LibGit2.Marshal (oidInC, textIn, textInPtr)
import LibGit2.Signature (sigMarshal)
import LibGit2.Types (Oid, Signature)

instance DefaultIn Text (PtrConst CChar -> lo) lo where
  defaultIn = textIn
instance DefaultIn Text (Ptr CChar -> lo) lo where
  defaultIn = textInPtr
instance DefaultIn Oid (PtrConst Git_oid -> lo) lo where
  defaultIn = oidInC
instance DefaultIn Signature (PtrConst Git_signature -> lo) lo where
  defaultIn = asArgumentC sigMarshal
