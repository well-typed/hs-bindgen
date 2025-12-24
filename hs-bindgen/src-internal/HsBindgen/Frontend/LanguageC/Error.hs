-- | Errors arising from interacting with @language-c@
--
-- Defined in its own module to avoid cyclic module dependenies.
module HsBindgen.Frontend.LanguageC.Error (
    Error(..)
  ) where

import GHC.Stack
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Error =
    -- | We encountered something unexpected in the AST from language-c
    --
    -- This indicates a bug or something not yet implemented.
    UpdateUnexpected CallStack String

    -- | We encountered something in the language-c AST we don't not support
    --
    -- It is useful to distinguish known-to-be-unsupported from
    -- unknown-to-be-supported ('UpdateUnexpected'): the latter indicates that
    -- it's simply something we haven't considered yet, the former is a
    -- conscious decision about features we currently don't want to support.
  | UpdateUnsupported String
  deriving stock (Show)

instance PrettyForTrace Error where
  prettyForTrace (UpdateUnexpected cs str) = PP.vsep [
        PP.hsep [
            "Encountered unexpected node in the language-c AST: "
          , PP.string str
          ]
      , PP.string (prettyCallStack cs)
      ]
  prettyForTrace (UpdateUnsupported err) =
        PP.hsep [
            "Unsupported: "
          , PP.string err
          ]
