-- | Generic TTG-style annotations
--
-- This module should only be used within the @HsBindgen.IR@ hierarchy.  From
-- outside the @HsBindgen.IR@ hierarchy, "HsBindgen.IR.Pass" should be used.
--
-- Intended for unqualified import.
--
-- > import HsBindgen.IR.Pass.Ann
module HsBindgen.IR.Pass.Ann (
    -- * Associated type families
    PassAnn(..)
    -- * Coercion
  , CoercePassAnn(..)
    -- * Defaults
  , NoAnn(..)
  ) where

import HsBindgen.Imports
import HsBindgen.IR.Pass.Definition

{-------------------------------------------------------------------------------
  Associated type families
-------------------------------------------------------------------------------}

class (
      -- For @Eq DeclKind@ in @DeclIndex@ construction
      Eq   (Ann "Enum"                 p)
    , Eq   (Ann "Flam"                 p)
    , Eq   (Ann "Function"             p)
    , Eq   (Ann "Global"               p)
    , Eq   (Ann "Struct"               p)
    , Eq   (Ann "StructField"          p)
    , Eq   (Ann "TypeFunArg"           p)
    , Eq   (Ann "TypecheckedMacroType" p)
    , Eq   (Ann "Typedef"              p)
    , Eq   (Ann "Union"                p)
    , Eq   (Ann "UnionField"           p)

      -- For de-duplicating types
    , Ord  (Ann "TypeFunArg"           p)

      -- For debugging
    , Show (Ann "Decl"                 p)
    , Show (Ann "Enum"                 p)
    , Show (Ann "Flam"                 p)
    , Show (Ann "Function"             p)
    , Show (Ann "Global"               p)
    , Show (Ann "Struct"               p)
    , Show (Ann "StructField"          p)
    , Show (Ann "TranslationUnit"      p)
    , Show (Ann "TypeFunArg"           p)
    , Show (Ann "TypecheckedMacroType" p)
    , Show (Ann "Typedef"              p)
    , Show (Ann "Union"                p)
    , Show (Ann "UnionField"           p)
    ) => PassAnn (p :: Pass) where

  -- | Generic TTG-style annotation
  --
  -- For single-constructor datatypes, the index can simply be the name of the
  -- datatype.  For multi-constructor datatypes, it should be @Type.Constr@ (or
  -- simply @Constr@ if that is unambiguous).
  type Ann (ix :: Symbol) p :: Star

{-------------------------------------------------------------------------------
  Coercion
-------------------------------------------------------------------------------}

class CoercePassAnn ix p p' where
  coercePassAnn :: Proxy '(ix, p, p') -> Ann ix p -> Ann ix p'

  default coercePassAnn ::
       (Ann ix p ~ Ann ix p')
    => Proxy '(ix, p, p') -> Ann ix p -> Ann ix p'
  coercePassAnn _ = id

{-------------------------------------------------------------------------------
  Defaults
-------------------------------------------------------------------------------}

data NoAnn = NoAnn
  deriving stock (Eq, Ord, Show)
