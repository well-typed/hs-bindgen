-- | Errors arising from interacting with @language-c@
--
-- Defined in its own module to avoid cyclic module dependencies.
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

    -- | We encountered something in the language-c AST that allows us to skip
    -- the current reparse
    --
    -- The reparser constructs an hs-bindgen AST from a language-c AST,
    -- inserting references to macro types. We can skip the current reparse if
    -- we are in a known case where no references to macro types can be inserted
    -- anymore, which would mean reparsing is just the identity funcion. Such is
    -- the case when we try to reparse a type that references a struct or union.
    -- In such cases skipping is useful because a reference to a *nested*
    -- struct\/union would otherwise incur an 'UpdateUnsupported' or
    -- 'UpdateUnexpected' error.
    --
    -- We reparse four types of declarations:
    -- * global variable declarations
    -- * typedef declarations
    -- * function declarations
    -- * struct/union field declarations
    --
    -- The types of nesting that can occur are:
    -- * struct/union (field) nested in global variable
    -- * struct/union (field) nested in typedef
    -- * struct/union (field) nested in function
    -- * struct/union (field) nested in struct/union field
    --
    -- An enclosing declaration can not contain both a reference to a (nested)
    -- struct or (nested) union, and a reference to a macro type. Therefore, we
    -- know that we can skip reparsing if we identify a reference to a struct or
    -- union.
    --
    -- === Example
    --
    -- For example, if we are reparsing typedef @foo@:
    --
    -- > #define MyInt int
    -- > #define MyConst const
    -- > typedef MyConst struct S { MyInt x; } * foo;
    --
    -- Even though @MyConst@ is referenced from @foo@, it is not parsed as a
    -- macro, so there is nothing to do for the reparser there. Then we can skip
    -- reparsing @foo@ once we see that @foo@ references struct @S@, because we
    -- will reparse the @x@ field of struct @S@ separately.
    --
  | UpdateSkipped String
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
  prettyForTrace (UpdateSkipped msg) =
        PP.hsep [
            "Skipped: "
          , PP.string msg
          ]

-- | Unsupported features are warnings
instance IsTrace Level Error where
  getDefaultLogLevel = \case
      UpdateUnexpected{}  -> Info
      UpdateUnsupported{} -> Info
      UpdateSkipped{}     -> Debug
  getSource  = const HsBindgen
  getTraceId _ = "parse-macro"
