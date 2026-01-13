module HsBindgen.Backend.Hs.Translation.Config (
    TranslationConfig(..)
  ) where

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Translation options
--
-- These options allow users to specify instances that /may/ be derived for all
-- structs, enums, and typedefs, along with the strategy to use.  Instances are
-- only derived when possible, however.  For example, an @Eq@ instance may only
-- be derived if all fields have @Eq@ instances.  Note that type classes that
-- @hs-bindgen@ generates instances for must not be included in this
-- configuration.
data TranslationConfig = TranslationConfig {
      -- | Default set of classes to derive for structs
      deriveStruct :: [(Hs.Strategy Hs.HsType, Inst.TypeClass)]

      -- | Default set of classes to derive for enums
    , deriveEnum :: [(Hs.Strategy Hs.HsType, Inst.TypeClass)]

      -- | Default set of classes to derive for typedefs
    , deriveTypedef :: [(Hs.Strategy Hs.HsType, Inst.TypeClass)]
    }
  deriving stock (Show, Eq, Generic)

instance Default TranslationConfig where
  def = TranslationConfig {
      deriveStruct = [
          (Hs.DeriveStock, Inst.Show)
        , (Hs.DeriveStock, Inst.Eq)
        ]
    , deriveEnum = [
          (Hs.DeriveStock, Inst.Eq)
        , (Hs.DeriveStock, Inst.Ord)
        ]
    , deriveTypedef = [
          (Hs.DeriveStock, Inst.Eq)
        , (Hs.DeriveStock, Inst.Ord)
        , (Hs.DeriveStock, Inst.Read)
        , (Hs.DeriveStock, Inst.Show)
        , (Hs.DeriveNewtype, Inst.Enum)
        , (Hs.DeriveNewtype, Inst.Ix)
        , (Hs.DeriveNewtype, Inst.Bounded)
        , (Hs.DeriveNewtype, Inst.Bits)
        , (Hs.DeriveNewtype, Inst.FiniteBits)
        , (Hs.DeriveNewtype, Inst.Floating)
        , (Hs.DeriveNewtype, Inst.Fractional)
        , (Hs.DeriveNewtype, Inst.Integral)
        , (Hs.DeriveNewtype, Inst.Num)
        , (Hs.DeriveNewtype, Inst.Prim)
        , (Hs.DeriveNewtype, Inst.Real)
        , (Hs.DeriveNewtype, Inst.RealFloat)
        , (Hs.DeriveNewtype, Inst.RealFrac)
        ]
    }
