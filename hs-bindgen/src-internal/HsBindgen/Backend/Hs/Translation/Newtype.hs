module HsBindgen.Backend.Hs.Translation.Newtype (
    newtypeDec
  , hasFFITypeDecs
  ) where

import Control.Monad.State qualified as State
import Data.Map.Strict qualified as Map

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Instances qualified as Hs
import HsBindgen.Backend.Hs.Translation.Monad (HsM)
import HsBindgen.Backend.Hs.Translation.Monad qualified as HsM
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
import HsBindgen.Language.Haskell qualified as Hs

-- | Smart constructor for creating a 'Hs.Newtype'
newtypeDec ::
     HasCallStack
  => Hs.Name Hs.NsTypeConstr
  -> Hs.Name Hs.NsConstr
  -> Hs.Field
  -> Origin.Decl Origin.Newtype
  -> Maybe HsDoc.Comment
  -> Set Inst.TypeClass -- ^ Candidate instances
  -> Set Inst.TypeClass -- ^ Known instances
  -> HsM Hs.Newtype
newtypeDec name constr field orig comment candidateInsts knownInsts = do
    hsNewtype <- aux <$> State.get
    State.modify' $ #instanceMap %~ Map.insert hsNewtype.name hsNewtype.instances
    pure hsNewtype
  where
    aux :: HsM.St -> Hs.Newtype
    aux transState = Hs.Newtype {
            name      = name
          , constr    = constr
          , field     = field
          , origin    = orig
          , instances = insts
          , comment   = comment
          }
      where
        resolvedInsts :: Set Inst.TypeClass
        resolvedInsts =
          Hs.getInstances
            transState.instanceMap
            (Just name)
            candidateInsts
            [field.typ]

        insts :: Set Inst.TypeClass
        insts = knownInsts <> resolvedInsts

hasFFITypeDecs ::
     Hs.Newtype
  -> [Hs.Decl l]
hasFFITypeDecs nt =
    [mk | Inst.HasFFIType `elem` nt.instances]
  where
    mk :: Hs.Decl l
    mk = Hs.DeclDeriveInstance Hs.DeriveInstance{
          strategy = Hs.DeriveNewtype
        , clss     = Inst.HasFFIType
        , name     = nt.name
        , comment  = Nothing
        }
