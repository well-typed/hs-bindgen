module HsBindgen.Frontend.Pass.Parse.IsPass (
    Parse
    -- * Macros
  , ReparseInfo(..)
  , invokedMacros
  , Tokens
    -- * Fields
  , FieldOrigin(..)
  , ExplicitFieldOrigin(..)
  , ImplicitFieldOrigin(..)
    -- * IsAnon
  , IsAnon(..)
  ) where

import Data.Set qualified as Set

import Clang.HighLevel.Types

import HsBindgen.Clang.Macros (MacroInvocation (name))
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Interface qualified as Macro

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Parse :: Pass
data Parse a

type family AnnParse (ix :: Symbol) :: Star where
  AnnParse "Function"    = ReparseInfo Tokens
  AnnParse "Global"      = ReparseInfo Tokens
  AnnParse "Struct"      = IsAnon
  AnnParse "StructField" = (ReparseInfo Tokens, FieldOrigin)
  AnnParse "Typedef"     = ReparseInfo Tokens
  AnnParse "Union"       = IsAnon
  AnnParse "UnionField"  = (ReparseInfo Tokens, FieldOrigin)
  AnnParse _             = NoAnn

instance IsPass Parse

instance PassId Parse where
  type Id Parse = C.PrelimDeclId

  idNameKind     _ = C.prelimDeclIdNameKind
  idSourceName   _ = C.prelimDeclIdSourceName
  idLocationInfo _ = C.prelimDeclIdLocationInfo

instance PassScopedName Parse

instance PassMacro Parse where
  type MacroBody Parse = Macro.Unresolved

instance PassExtBinding Parse

instance PassCommentDecl Parse

instance PassAnn Parse where
  type Ann ix Parse = AnnParse ix

instance PassMsg Parse where
  type Msg Parse = C.WithLocationInfo ImmediateParseMsg

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

data ReparseInfo tokens =
    -- | We need to reparse this declaration (to deal with macros)
    --
    -- We do not use this for macro declarations _themselves_ (see
    -- 'ParsedMacro').
    ReparseNeeded
      tokens
      -- ^ Original tokens of declaration without macro expansions
      (NonEmpty MacroInvocation)
      -- ^ Expanded macros

    -- | This declaration does not use macros, so no need to reparse
  | ReparseNotNeeded
  deriving stock (Show, Eq, Ord)

-- | Names of expanded macros
invokedMacros :: NonEmpty MacroInvocation -> Set Text
invokedMacros = foldl' (\acc inv -> Set.insert inv.name acc) Set.empty

type Tokens = [Token TokenSpelling]

{-------------------------------------------------------------------------------
  Fields
-------------------------------------------------------------------------------}

-- | How a struct or union field came to be
data FieldOrigin =
    ExplicitParsed ExplicitFieldOrigin
  | ImplicitGenerated ImplicitFieldOrigin
  deriving stock (Show, Eq, Ord)

-- | An explicit struct or union field is parsed directly from a C header file
--
-- Named fields are always parsed directly from a C header file.
data ExplicitFieldOrigin = ExplicitFieldOrigin
  deriving stock (Show, Eq, Ord)

-- | An implicit struct or union field is generated for nested anonymous structs
-- and unions
--
-- We track this information because @libclang@ does not expose information
-- about implicit fields, so we have to derive the information ourselves using a
-- custom algorithm. See the "HsBindgen.Frontend.Pass.Parse.Decl.ImplicitFields"
-- module for the algorithm.
newtype ImplicitFieldOrigin = ImplicitFieldOrigin {
    -- | The name of the first field of the anonymous object
    --
    -- The offset from the enclosing object to an anonymous struct or union is
    -- equal to the offset from the enclosing object to the first field of the
    -- anonymous struct or union. The first field can also be an indirect field
    -- if there are multiple levels of nested anonymous structs/unions.
    field :: C.ScopedName
  }
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  IsAnon
-------------------------------------------------------------------------------}

-- | A struct or union is anonymous if it is untagged and if it is referenced by
-- a single unnamed field.
newtype IsAnon = IsAnon { isAnon :: Bool }
  deriving stock (Show, Eq, Ord)
