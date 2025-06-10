module HsBindgen.Frontend.Pass.RenameAnon.IsPass (
    RenameAnon
  , RenamedTypedefRef(..)
  ) where

import HsBindgen.Frontend.AST.Internal (ValidPass)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Graph.UseDef (UseDefGraph)
import HsBindgen.Frontend.NonSelectedDecls (NonSelectedDecls)
import HsBindgen.Frontend.Pass
import HsBindgen.Imports
import HsBindgen.Language.C

{-------------------------------------------------------------------------------
  Pass definition
-------------------------------------------------------------------------------}

type RenameAnon :: Pass
data RenameAnon a deriving anyclass ValidPass

type family AnnRenameAnon ix where
  AnnRenameAnon "TranslationUnit" = (UseDefGraph, NonSelectedDecls)
  AnnRenameAnon _                 = NoAnn

instance IsPass RenameAnon where
  type Id         RenameAnon = CName
  type FieldName  RenameAnon = CName
  type TypedefRef RenameAnon = RenamedTypedefRef RenameAnon
  type MacroBody  RenameAnon = C.CheckedMacro RenameAnon
  type Ann ix     RenameAnon = AnnRenameAnon ix

{-------------------------------------------------------------------------------
  Annotations
-------------------------------------------------------------------------------}

-- | Reference to a typedef type
data RenamedTypedefRef p =
    -- | Regular reference
    TypedefRegular (Id p)

    -- When we have a typedef around an anonymous declaration:
    --
    -- > typedef struct {
    -- >   int x;
    -- >   int y;
    -- > } foo;
    --
    -- we don't want to generate two types in Haskell (one for the anonymous
    -- struct and then a separate newtype around it), but just the one (for the
    -- struct, with the name from the typedef).
    --
    -- A similar situation arises when the struct tag is /already/ equal to the
    -- name of the typedef:
    --
    -- > typedef struct foo {
    -- >   int x;
    -- >   int y;
    -- > } foo;
    --
    -- We remove the declaration of these typedefs from the AST entirely (we do
    -- not need to generate code for them). We record the original C name
    -- (without a corresponding Haskell name) as well as the type that replaced
    -- the reference with.
  | TypedefSquashed CName (C.Type p)
  deriving stock (Show, Eq, Generic)
