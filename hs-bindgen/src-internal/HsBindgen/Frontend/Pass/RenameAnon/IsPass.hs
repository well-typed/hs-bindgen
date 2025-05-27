module HsBindgen.Frontend.Pass.RenameAnon.IsPass (
    RenameAnon
  , CName(..)
  , SquashedTypedef(..)
  ) where

import HsBindgen.Frontend.AST
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Imports
import HsBindgen.Frontend.Graph.UseDef (UseDefGraph)
import HsBindgen.Frontend.Pass.Parse.IsPass

{-------------------------------------------------------------------------------
  Pass definition
-------------------------------------------------------------------------------}

type RenameAnon :: Pass
data RenameAnon a deriving anyclass ValidPass

type family AnnRenameAnon ix where
  AnnRenameAnon "TranslationUnit" = UseDefGraph Parse
  AnnRenameAnon "TypeTypedef"     = SquashedTypedef
  AnnRenameAnon _                 = NoAnn

instance IsPass RenameAnon where
  type Id     RenameAnon = CName
  type Macro  RenameAnon = CheckedMacro
  type Ann ix RenameAnon = AnnRenameAnon ix

{-------------------------------------------------------------------------------
  Identity
-------------------------------------------------------------------------------}

newtype CName = CName Text
  deriving newtype (Show, Eq, Ord, IsString, Semigroup)

{-------------------------------------------------------------------------------
  Annotations
-------------------------------------------------------------------------------}

-- | Will be have squashed this typedef when we generated the Haskell code?
--
-- When we have a typedef around an anonymous declaration:
--
-- > typedef struct {
-- >   int x;
-- >   int y;
-- > } foo;
--
-- we don't want to generate two types in Haskell (one for the anonymous struct
-- and then a separate newtype around it), but just the one (for the struct,
-- with the name from the typedef).
--
-- A similar situation arises when the struct tag is /already/ equal to the
-- name of the typedef:
--
-- > typedef struct foo {
-- >   int x;
-- >   int y;
-- > } foo;
--
-- We remove the declaration of these typedefs from the AST entirely (we do not
-- need to generate code for them), and mark any /references/ to these typedefs
-- as 'SquashedTypedef'.
data SquashedTypedef =
    KeptTypedef
  | SquashedTypedef
  deriving stock (Show)


