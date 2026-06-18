-- | In its own module to prevent cyclic dependencies
module HsBindgen.Frontend.Pass.ResolveBindingSpecs.ResolvedExtBinding (
    ResolvedExtBinding (..)
  , extDeclIdPair
  ) where

import Clang.HighLevel.Types

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Naming (DeclId, DeclIdPair (..))
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

data ResolvedExtBinding = ResolvedExtBinding{
      -- | C declaration for which we are using this binding
      cName :: DeclId

      -- | Source location of C declaration for which we are using this binding
    , locs :: NonEmpty SingleLoc

      -- | The Haskell type which will be used
    , hsName :: Hs.ExtRef

      -- | Additional information about the C type
    , cSpec :: BindingSpec.CTypeSpec

      -- | Additional information about the Haskell type
    , hsSpec :: BindingSpec.HsTypeSpec
    }
  deriving stock (Show, Eq, Ord, Generic)

extDeclIdPair :: ResolvedExtBinding -> DeclIdPair
extDeclIdPair ext = DeclIdPair{
      cName  = ext.cName
    , hsName = Hs.demoteNs ext.hsName.name
    }
