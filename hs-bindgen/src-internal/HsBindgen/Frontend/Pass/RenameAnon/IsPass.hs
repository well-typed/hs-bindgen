module HsBindgen.Frontend.Pass.RenameAnon.IsPass (
    RenameAnon
  , CName(..)
  , SquashedTypedef(..)
  ) where

import HsBindgen.Frontend.AST
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Pass definition
-------------------------------------------------------------------------------}

type RenameAnon :: Pass
data RenameAnon a

type family AnnRenameAnon ix where
  AnnRenameAnon "TypeTypedef" = SquashedTypedef
  AnnRenameAnon ix            = Ann ix (Previous RenameAnon)

instance IsPass RenameAnon where
  type Previous RenameAnon = HandleMacros
  type Id       RenameAnon = CName
  type Ann ix   RenameAnon = AnnRenameAnon ix

instance ShowPass RenameAnon

{-------------------------------------------------------------------------------
  Identity
-------------------------------------------------------------------------------}

newtype CName = CName Text
  deriving newtype (Show, IsString, Semigroup)

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


