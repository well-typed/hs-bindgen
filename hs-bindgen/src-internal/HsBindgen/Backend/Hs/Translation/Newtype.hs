module HsBindgen.Backend.Hs.Translation.Newtype (
    newtypeDec
  ) where

import Control.Monad.State qualified as State
import Data.Map.Strict qualified as Map

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Instances qualified as Hs
import HsBindgen.Backend.Hs.Translation.State (TranslationState)
import HsBindgen.Backend.Hs.Translation.State qualified as State
import HsBindgen.Backend.Hs.Translation.Type qualified as Hs
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
    State.modifyInstanceMap' $
      Map.insert hsNewtype.newtypeName
                 hsNewtype.newtypeInstances
    State.modifyNewtypeMap' $
      Map.insert hsNewtype.newtypeName
                 hsNewtype.newtypeField.fieldType
    pure hsNewtype
  where
    aux :: TranslationState -> Hs.Newtype
    aux transState = Hs.Newtype {
            newtypeName = name
          , newtypeConstr = constr
          , newtypeField = field
          , newtypeOrigin = orig
          , newtypeInstances = insts
          , newtypeFFIType = Hs.toFFIType (transState.newtypeMap) fieldType
          , newtypeComment = comment
          }
      where
        fieldType :: Hs.HsType
        fieldType = Hs.fieldType field

        resolvedInsts :: Set Hs.TypeClass
        resolvedInsts = Hs.getInstances (State.instanceMap transState) (Just name) candidateInsts [fieldType]

        insts :: Set Hs.TypeClass
        insts = knownInsts <> resolvedInsts
