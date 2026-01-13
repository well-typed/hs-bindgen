-- | Instance resolution types
--
-- Intended for qualified import.
--
-- > import HsBindgen.Instances qualified as Inst
module HsBindgen.Instances (
    -- * Core types
    TypeClass(..)
  , Dependency(..)
  , Strategy(..)
    -- * Supported instances
  , SupportedInstances(..)
  , SupportedStrategies(..)
  ) where

import Data.Aeson.Types qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Ord qualified as Ord
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Imports
import HsBindgen.Util.Tracer (PrettyForTrace (prettyForTrace))

{-------------------------------------------------------------------------------
  Core types
-------------------------------------------------------------------------------}

-- | Type class
data TypeClass =
    Bitfield
  | Bits
  | Bounded
  | CEnum
  | Enum
  | Eq
  | FiniteBits
  | Flam_Offset
  | Floating
  | Fractional
  | HasCBitField  -- Indicates instances for all bit fields
  | HasCField     -- Indicates instances for all non-bit fields
  | HasFFIType
  | HasField      -- Indicates instances for all fields
  | Integral
  | Ix
  | Num
  | Ord
  | Prim
  | Read
  | ReadRaw
  | Real
  | RealFloat
  | RealFrac
  | SequentialCEnum
  | Show
  | StaticSize
  | Storable
  | WriteRaw
  deriving stock (Eq, Generic, Read, Show)

-- Order lexicographically, even if somebody adds a constructor out of place
instance Ord TypeClass where
  compare = Ord.comparing show

instance PrettyForTrace TypeClass where
  prettyForTrace = PP.show

--------------------------------------------------------------------------------

-- | Relationship between an instance and dependencies of a type
data Dependency =
    -- | An instance depends on instances of dependencies
    Dependent
  | -- | An instance does not depend on instances of dependencies
    Independent
  deriving stock (Eq, Generic, Show)

--------------------------------------------------------------------------------

-- | Strategy used to generate/derive an instance
data Strategy =
    -- | @hs-bindgen@ decides how to generate/derive an instance
    HsBindgen
  | -- | Derive an instance using the @newtype@ strategy
    Newtype
  | -- | Derive an instance using the @stock@ strategy
    Stock
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

instance Aeson.FromJSON Strategy where
  parseJSON = Aeson.withText "Strategy" $ \case
    "hs-bindgen" -> return HsBindgen
    "newtype"    -> return Newtype
    "stock"      -> return Stock
    t            -> Aeson.parseFail $ "unknown strategy: " ++ Text.unpack t

instance Aeson.ToJSON Strategy where
  toJSON = Aeson.String . \case
    HsBindgen -> "hs-bindgen"
    Newtype   -> "newtype"
    Stock     -> "stock"

instance PrettyForTrace Strategy where
  prettyForTrace = PP.show

{-------------------------------------------------------------------------------
  Supported instances
-------------------------------------------------------------------------------}

-- | Supported instances
--
-- This type tracks the instances that may be generated/derived in generated
-- bindings.  The 'Default' instance of this type specifies the instances and
-- strategies that are supported by this version of @hs-bindgen@.
--
-- If we decide to allow users to configure this, to change which instances are
-- generated/derived by default, then we should probably provide an API that
-- ensures that a subset of the supported API is configured.
--
-- Per-type configuration, overriding these defaults, is done using prescriptive
-- binding specifications.
data SupportedInstances = SupportedInstances {
      -- | Supported instances for @struct@ types
      struct  :: Map TypeClass SupportedStrategies
    , -- | Supported instances for @union@ types
      union   :: Map TypeClass SupportedStrategies
    , -- | Supported instances for @enum@ types
      enum    :: Map TypeClass SupportedStrategies
    , -- | Supported instances for @typedef@ types
      typedef :: Map TypeClass SupportedStrategies
    }
  deriving stock (Show)

-- | Supported strategies for a specific instance
data SupportedStrategies = SupportedStrategies {
      -- | Relationship between the instance and dependencies of the type
      dependency :: Dependency

    , -- | Supported strategies to generate/derive the instance
      --
      -- This set must not be empty.
      strategies :: Set Strategy

    , -- | Default strategy to generate/derive the instance
      --
      -- A 'Just' value indicates that the instance may be generated/derived by
      -- default, with the specified strategy.  A 'Nothing' value indicates that
      -- the instance may /not/ be generated/derived by default.
      --
      -- If specified, the strategy must be a member of 'strategies'.
      defStrategy :: Maybe Strategy
    }
  deriving stock (Show)

instance Default SupportedInstances where
  def =
    SupportedInstances{
        struct = Map.fromList [
            mkDef Eq           Dependent   Stock     []
          , mkDef HasCBitField Independent HsBindgen []
          , mkDef HasCField    Independent HsBindgen []
          , mkDef HasField     Independent HsBindgen []
          , mkDef Flam_Offset  Independent HsBindgen []
          , mkOpt Ord          Dependent             [Stock]
          , mkDef Prim         Dependent   HsBindgen []
          , mkDef Show         Dependent   Stock     []
          , mkDef Storable     Dependent   HsBindgen []
          ]
      , union = Map.fromList [
            mkDef HasCBitField Independent HsBindgen []
          , mkDef HasCField    Independent HsBindgen []
          , mkDef HasField     Independent HsBindgen []
          , mkDef Prim         Independent HsBindgen []
          , mkDef Storable     Independent HsBindgen []
          ]
      , enum = Map.fromList [
            mkOpt Bounded         Independent           [HsBindgen, Newtype]
          , mkDef CEnum           Independent HsBindgen []
          , mkOpt Enum            Independent           [HsBindgen, Newtype]
          , mkDef Eq              Dependent   Stock     []
          , mkDef HasCField       Independent HsBindgen []
          , mkDef HasFFIType      Dependent   Newtype   []
          , mkDef HasField        Independent HsBindgen []
          , mkDef Ord             Dependent   Stock     []
          , mkDef Prim            Independent HsBindgen []
          , mkDef Read            Independent HsBindgen [Newtype, Stock]
          , mkDef SequentialCEnum Independent HsBindgen []
          , mkDef Show            Independent HsBindgen [Newtype, Stock]
          , mkDef Storable        Independent HsBindgen []
          ]
      , typedef = Map.fromList [
            mkDef Bitfield   Dependent   Newtype   []
          , mkDef Bits       Dependent   Newtype   []
          , mkDef Bounded    Dependent   Newtype   []
          , mkDef Enum       Dependent   Newtype   []
          , mkDef Eq         Dependent   Stock     []
          , mkDef FiniteBits Dependent   Newtype   []
          , mkDef Floating   Dependent   Newtype   []
          , mkDef Fractional Dependent   Newtype   []
          , mkDef HasCField  Independent HsBindgen []
          , mkDef HasFFIType Dependent   Newtype   []
          , mkDef HasField   Independent HsBindgen []
          , mkDef Integral   Dependent   Newtype   []
          , mkDef Ix         Dependent   Newtype   []
          , mkDef Num        Dependent   Newtype   []
          , mkDef Ord        Dependent   Stock     []
          , mkDef Prim       Dependent   Newtype   []
          , mkDef Read       Dependent   Stock     [Newtype]
          , mkDef Real       Dependent   Newtype   []
          , mkDef RealFloat  Dependent   Newtype   []
          , mkDef RealFrac   Dependent   Newtype   []
          , mkDef Show       Dependent   Stock     [Newtype]
          , mkDef Storable   Dependent   Newtype   []
          ]
      }
    where
      mkDef ::
           TypeClass
        -> Dependency
        -> Strategy
        -> [Strategy]
        -> (TypeClass, SupportedStrategies)
      mkDef cls dep strat strats = (cls,) $ SupportedStrategies{
          dependency  = dep
        , strategies  = Set.fromList $ strat : strats
        , defStrategy = Just strat
        }

      mkOpt ::
           TypeClass
        -> Dependency
        -> [Strategy]
        -> (TypeClass, SupportedStrategies)
      mkOpt cls dep strats = (cls,) $ SupportedStrategies{
          dependency  = dep
        , strategies  = Set.fromList strats
        , defStrategy = Nothing
        }
