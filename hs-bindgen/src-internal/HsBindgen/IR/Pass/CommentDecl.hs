-- | Declaration comments
--
-- This module should only be used within the @HsBindgen.IR@ hierarchy.  From
-- outside the @HsBindgen.IR@ hierarchy, "HsBindgen.IR.Pass" should be used.
--
-- Intended for unqualified import.
--
-- > import HsBindgen.IR.Pass.CommentDecl
module HsBindgen.IR.Pass.CommentDecl (
    -- * Associated type families
    PassCommentDecl(..)
    -- * Coercion
  , CoercePassCommentDecl(..)
  ) where

import HsBindgen.Imports
import HsBindgen.IR.Pass.Definition

{-------------------------------------------------------------------------------
  Associated type families
-------------------------------------------------------------------------------}

-- | Declaration comments vary across passes
class (
      Eq   (CommentDecl p)
    , Show (CommentDecl p)  -- For debugging
    ) => PassCommentDecl (p :: Pass) where

  -- | Type of comments attached to declarations
  --
  -- 1. Before 'HsBindgen.Frontend.Pass.EnrichComments.IsPass.EnrichComments',
  --   this is '()', to statically encode that comments cannot exist yet.
  -- 2. After 'HsBindgen.Frontend.Pass.EnrichComments.IsPass.EnrichComments',
  --   this is @'Maybe' ('HsBindgen.IR.C.Decl.Comment' p)@.
  type CommentDecl p :: Star
  type CommentDecl p = ()

{-------------------------------------------------------------------------------
  Coercion
-------------------------------------------------------------------------------}

class CoercePassCommentDecl (p :: Pass) (p' :: Pass) where
  coercePassCommentDecl :: Proxy '(p, p') -> CommentDecl p -> CommentDecl p'

  default coercePassCommentDecl ::
       (CommentDecl p ~ CommentDecl p')
    => Proxy '(p, p') -> CommentDecl p -> CommentDecl p'
  coercePassCommentDecl _ = id
