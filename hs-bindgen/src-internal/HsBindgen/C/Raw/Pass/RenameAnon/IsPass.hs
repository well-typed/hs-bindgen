module HsBindgen.C.Raw.Pass.RenameAnon.IsPass (
    Renamed
  , CName(..)
  ) where

import HsBindgen.C.Raw.Pass
import HsBindgen.C.Raw.Pass.Parse
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Pass definition
-------------------------------------------------------------------------------}

type Renamed :: Pass
data Renamed a

instance IsPass Renamed where
  type Previous Renamed = Parsed
  type Id       Renamed = CName

{-------------------------------------------------------------------------------
  Identity
-------------------------------------------------------------------------------}

newtype CName = CName Text
  deriving newtype (Show, IsString, Semigroup)

