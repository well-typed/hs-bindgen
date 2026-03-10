-- | Results from the parser proper
module HsBindgen.Frontend.Pass.Parse.Result (
    ParseResult(..)
  , ParseClassification(..)
  , ParseSuccess(..)
  , ParseNotAttempted(..)
    -- * Convenience constructors
  , parseSucceed
  , parseSucceedWith
  , parseDoNotAttempt
    -- * Query
  , getParseResultMaybeDecl
  , getParseResultEitherDecl
  ) where

import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Parse result
--
-- NOTE: This does /NOT/ depend on the @Parse@ pass specifically: we transform
-- these results in the @AssignAnonIds@ pass.
data ParseResult p = ParseResult{
      id             :: Id p
    , loc            :: SingleLoc
    , classification :: ParseClassification p
    }

deriving stock instance IsPass p => Show (ParseResult p)

data ParseClassification p =
    ParseResultSuccess      (ParseSuccess p)
  | ParseResultNotAttempted ParseNotAttempted
  | ParseResultFailure      DelayedParseMsg
  deriving stock (Show, Generic)

instance PrettyForTrace (ParseClassification p) where
  prettyForTrace = \case
    ParseResultSuccess      x -> prettyForTrace x
    ParseResultNotAttempted x -> prettyForTrace x
    ParseResultFailure msg    -> PP.hang "Parse failure:" 2 $
      prettyForTrace msg

data ParseSuccess p = ParseSuccess {
      decl             :: C.Decl p
    , delayedParseMsgs :: [DelayedParseMsg]
    }
  deriving stock (Show, Generic)

-- | Why did we not attempt to parse a declaration?
data ParseNotAttempted =
    -- | We unexpectedly did not attempt to parse a declaration because it is
    -- reported "unavailable".
    DeclarationUnavailable
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance IsPass p => PrettyForTrace (ParseResult p) where
  prettyForTrace result =
      prettyForTrace $ WithLocationInfo{
          loc = idLocationInfo (Proxy @p) result.id [result.loc]
        , msg = result.classification
        }

instance PrettyForTrace (ParseSuccess p) where
  prettyForTrace success =
      if null success.delayedParseMsgs then
        "Parse success"
      else
        PP.hang "Parse success with messages:" 2 $
          PP.vcat $
          map prettyForTrace success.delayedParseMsgs

instance PrettyForTrace ParseNotAttempted where
  prettyForTrace = PP.hang "Parse not attempted: " 2 . \case
    DeclarationUnavailable   -> "Declaration is 'unavailable' on this platform"

{-------------------------------------------------------------------------------
  Convenience constructors
-------------------------------------------------------------------------------}

-- | Assemble a parse success
parseSucceed :: C.Decl p -> ParseResult p
parseSucceed = parseSucceedWith []

-- | Assemble a parse success with delayed parse messages
parseSucceedWith :: [DelayedParseMsg] -> C.Decl p -> ParseResult p
parseSucceedWith msgs decl = ParseResult{
     id             = decl.info.id
   , loc            = decl.info.loc
   , classification = ParseResultSuccess ParseSuccess{
         decl             = decl
       , delayedParseMsgs = msgs
       }
    }

-- | Assemble a "parse not attempted" with a reason
parseDoNotAttempt :: C.DeclInfo p -> ParseNotAttempted -> ParseResult p
parseDoNotAttempt info reason = ParseResult{
      id              = info.id
    , loc             = info.loc
    , classification = ParseResultNotAttempted reason
    }

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

getParseResultMaybeDecl :: ParseResult p -> Maybe (C.Decl p)
getParseResultMaybeDecl result =
    case result.classification of
      ParseResultSuccess success -> Just $ success.decl
      _other                     -> Nothing

getParseResultEitherDecl :: ParseResult p -> Either (ParseResult p) (C.Decl p)
getParseResultEitherDecl result =
    maybe (Left result) Right $ getParseResultMaybeDecl result
