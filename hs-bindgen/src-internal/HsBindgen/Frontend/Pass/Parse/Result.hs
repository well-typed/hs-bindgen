-- | Results from the parser proper
module HsBindgen.Frontend.Pass.Parse.Result (
    ParseResult(..)
  , ParseClassification(..)
  , ParseSuccess(..)
  , ParseNotAttempted(..)
  , ParseFailure(..)
    -- * Convenience constructors
  , parseSucceed
  , parseSucceedWith
  , parseDoNotAttempt
  , parseFail
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

data ParseClassification p =
    ParseResultSuccess      (ParseSuccess p)
  | ParseResultNotAttempted ParseNotAttempted
  | ParseResultFailure      ParseFailure
  deriving stock (Show, Generic)
  deriving anyclass (PrettyForTrace)

data ParseSuccess p = ParseSuccess {
      decl             :: C.Decl p
    , delayedParseMsgs :: [DelayedParseMsg]
    }
  deriving stock (Show, Generic)

-- | Why did we not attempt to parse a declaration?
data ParseNotAttempted =
    -- | We do not parse builtin declarations.
    DeclarationBuiltin

    -- | We unexpectedly did not attempt to parse a declaration because it is
    -- reported "unavailable".
  | DeclarationUnavailable

    -- | Declarations that do not match the parse predicate.
    --
    -- For example, we may provide external bindings for skipped declarations.
    -- We do /not/ support external bindings for /anonymous/ non-parsed
    -- declarations; /if/ you want to provide an external binding for some local
    -- type, for example
    --
    -- > struct rect {
    -- >   struct { int x; int y; } bottomleft;
    -- >   struct { int x; int y; } topright;
    -- > };
    --
    -- then you need to make sure that you /traverse/ @rect@, so that the
    -- @NameAnon@ pass can do its work.
  | ParsePredicateNotMatched
  deriving stock (Show, Eq, Ord)

-- | Declarations that match the parse predicate but that we fail to parse and
-- reify
--
-- We need this information when selecting declarations: Does the user want to
-- select declarations, we have failed to parse?
newtype ParseFailure = ParseFailure {
      msg :: DelayedParseMsg
    }
  deriving stock (Show, Generic)
  deriving newtype (IsTrace Level)

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
  prettyForTrace x = PP.hang "Parse not attempted: " 2 $
      case x of
        DeclarationBuiltin       -> "Builtin declaration"
        DeclarationUnavailable   -> "Declaration is 'unavailable' on this platform"
        ParsePredicateNotMatched -> "Parse predicate did not match"

instance PrettyForTrace ParseFailure where
  prettyForTrace failure = PP.hang "Parse failure:" 2 $
      prettyForTrace failure.msg

{-------------------------------------------------------------------------------
  Convenience constructors
-------------------------------------------------------------------------------}

parseSucceed :: C.Decl p -> ParseResult p
parseSucceed = parseSucceedWith []

parseSucceedWith :: [DelayedParseMsg] -> C.Decl p -> ParseResult p
parseSucceedWith delayedParseMsgs decl = ParseResult{
     id             = decl.info.id
   , loc            = decl.info.loc
   , classification = ParseResultSuccess ParseSuccess{
                          decl
                        , delayedParseMsgs
                        }
    }

parseDoNotAttempt :: C.DeclInfo p -> ParseNotAttempted -> ParseResult p
parseDoNotAttempt info reason = ParseResult{
      id              = info.id
    , loc             = info.loc
    , classification = ParseResultNotAttempted reason
    }

parseFail :: Id p -> SingleLoc -> DelayedParseMsg -> ParseResult p
parseFail declId declLoc msg = ParseResult{
      id             = declId
    , loc            = declLoc
    , classification = ParseResultFailure $ ParseFailure msg
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
