-- TODO: This should obsolete 'ProduceCandidate'
module HsBindgen.Frontend.Pass.RenameAnon.ProduceCName (
    nameForAnon
  ) where

import HsBindgen.Errors
import HsBindgen.Frontend.Graph.DefUse (UseOfDecl (..))
import HsBindgen.Frontend.Graph.UseDef (Usage (..), ValOrRef (..))
import HsBindgen.Frontend.Pass.RenameAnon.IsPass

-- | Construct name for anonymous declaration
--
-- TODO: We should be able to configure how this constructs the name
-- (.. or should we? Maybe the only configuration required should be the
-- mapping to Haskell names? If we do that, that would also mean that binding
-- specifications for anonymous declarations would no longer depend on a
-- choice of name mangler, which would be much better.)
nameForAnon :: UseOfDecl -> CName
nameForAnon = \case
      UsedByNamed (UsedInTypedef ByValue) (name, _namespace) ->
        CName name
      UsedByNamed (UsedInTypedef ByRef) (name, _namespace) ->
        CName name <> "_Deref"
      UsedByNamed (UsedInField _valOrRef field) (name, _namespace) ->
        CName name <> "_" <> CName field
      UsedByNamed (UsedInFunction _valOrRef) (name, _namespace) ->
        CName name
      UsedByAnon (UsedInTypedef _valOrRef) _useOfAnon ->
        panicPure $ "nameForAnon: unexpected anonymous typedef"
      UsedByAnon (UsedInField _valOrRef field) useOfAnon ->
        nameForAnon useOfAnon <> "_" <> CName field
      UsedByAnon (UsedInFunction _valOrRef) _useOfAnon ->
        panicPure $ "nameForAnon: unexpected anonymous function argument or return type"
