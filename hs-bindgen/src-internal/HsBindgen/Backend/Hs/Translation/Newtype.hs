module HsBindgen.Backend.Hs.Translation.Newtype (
    newtypeDec
  , hasBaseForeignTypeDecs
  ) where

import Control.Monad.State qualified as State
import Data.Map.Strict qualified as Map

import HsBindgen.Backend.Hs.AST (TypeClass (..))
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Instances qualified as Hs
import HsBindgen.Backend.Hs.Translation.State (TranslationState)
import HsBindgen.Backend.Hs.Translation.State qualified as State
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

-- | Smart constructor for creating a 'Hs.Newtype'
newtypeDec ::
     State.MonadState TranslationState m
  => Hs.Name Hs.NsTypeConstr
  -> Hs.Name Hs.NsConstr
  -> Hs.Field
  -> Origin.Decl Origin.Newtype
  -> Maybe HsDoc.Comment
  -> Set Hs.TypeClass -- ^ Candidate instances
  -> Set Hs.TypeClass -- ^ Known instances
  -> m Hs.Newtype
newtypeDec name constr field orig comment candidateInsts knownInsts = do
    hsNewtype <- aux <$> State.get
    State.modify' $ #instanceMap %~ Map.insert hsNewtype.name hsNewtype.instances
    pure hsNewtype
  where
    aux :: TranslationState -> Hs.Newtype
    aux transState = Hs.Newtype {
            name      = name
          , constr    = constr
          , field     = field
          , origin    = orig
          , instances = insts
          , comment   = comment
          }
      where
        resolvedInsts :: Set Hs.TypeClass
        resolvedInsts = Hs.getInstances transState.instanceMap (Just name) candidateInsts [field.typ]

        insts :: Set Hs.TypeClass
        insts = knownInsts <> resolvedInsts

hasBaseForeignTypeDecs ::
     Hs.Newtype
  -> [Hs.Decl]
hasBaseForeignTypeDecs nt =
    [mk | HasBaseForeignType `elem` nt.instances]
  where
    mk :: Hs.Decl
    mk = Hs.DeclDeriveInstance Hs.DeriveInstance{
          strategy = Hs.DeriveNewtype
        , clss     = HasBaseForeignType
        , name     = nt.name
        , comment  = Nothing
        }
