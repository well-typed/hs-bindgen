module HsBindgen.Backend.Hs.Translation.Newtype (
    newtypeDec
  , hasFFITypeDecs
  ) where

import Control.Monad.State qualified as State
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Instances qualified as Hs
import HsBindgen.Backend.Hs.Translation.State (TranslationState)
import HsBindgen.Backend.Hs.Translation.State qualified as State
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
import HsBindgen.Language.Haskell qualified as Hs

-- | Smart constructor for creating a 'Hs.Newtype'
newtypeDec ::
     State.MonadState TranslationState m
  => Hs.Name Hs.NsTypeConstr
  -> Hs.Name Hs.NsConstr
  -> Hs.Field
  -> Origin.Decl Origin.Newtype
  -> Maybe HsDoc.Comment
  -> Set Inst.TypeClass -- ^ Candidate instances
  -> Set Inst.TypeClass -- ^ Known instances
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
        resolvedInsts :: Set Inst.TypeClass
        resolvedInsts = Set.unions [
              Hs.getInstances transState.instanceMap (Just name) candidateInsts [field.typ]
            , resolvedIsArrayInst
            ]

        resolvedIsArrayInst :: Set Inst.TypeClass
        resolvedIsArrayInst
          | Inst.IsArray `Set.member` candidateInsts
          , Hs.hasInstance_IsArray transState.instanceMap (Just name) field.typ
          = Set.singleton Inst.IsArray
          | otherwise
          = Set.empty

        insts :: Set Inst.TypeClass
        insts = knownInsts <> resolvedInsts

hasFFITypeDecs ::
     Hs.Newtype
  -> [Hs.Decl]
hasFFITypeDecs nt =
    [mk | Inst.HasFFIType `elem` nt.instances]
  where
    mk :: Hs.Decl
    mk = Hs.DeclDeriveInstance Hs.DeriveInstance{
          strategy = Hs.DeriveNewtype
        , clss     = Inst.HasFFIType
        , name     = nt.name
        , comment  = Nothing
        }
