-- | Simplification pass on the "HsBindgen.Backend.SHs.AST" representation
module HsBindgen.Backend.SHs.Simplify (simplifySHs) where

import Data.Either (partitionEithers)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import HsBindgen.Backend.Category
import HsBindgen.Backend.Hs.AST.Strategy
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.SHs.AST
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

simplifySHs ::
     ByCategory_ ([CWrapper], [SDecl])
  -> ByCategory_ ([CWrapper], [SDecl])
simplifySHs = fmap (\(x, y) -> (x, go y))
  where
    go :: [SDecl] -> [SDecl]
    go decls =
        let (decls', simpleInstances) =
              second (Map.fromListWith (<>)) $
                partitionMaybe toSimpleInstances decls
        in reconstruct simpleInstances decls'

reconstruct ::
     Map (Hs.Name Hs.NsTypeConstr) SimpleInstances
  -> [SDecl] -> [SDecl]
reconstruct simpleInstances = map aux
  where
    aux :: SDecl -> SDecl
    aux = \case
        DRecord record -> DRecord $
          record & #deriv %~ (++ instancesFor record.typ)
        DNewtype newtyp -> DNewtype $
          newtyp & #deriv %~ (++ instancesFor newtyp.name)
        otherDecl ->
          otherDecl

    instancesFor ::
         Hs.Name 'Hs.NsTypeConstr
      -> [(Strategy ClosedType, [Inst.TypeClass])]
    instancesFor =
          maybe [] fromSimpleInstances
        . flip Map.lookup simpleInstances

{-------------------------------------------------------------------------------
  Simple instances
-------------------------------------------------------------------------------}

-- | Simple instances are instances without special constraints
--
-- We group these per strategy, so that we can generate the most readable code.
data SimpleInstances = SimpleInstances{
      strategyStock   :: Set Inst.TypeClass
    , strategyNewtype :: Set Inst.TypeClass
    }
  deriving stock (Show)

fromSimpleInstances :: SimpleInstances -> [(Strategy ClosedType, [Inst.TypeClass])]
fromSimpleInstances instances =
    filter (not . null . snd) $ [
        (DeriveStock   , Set.toList instances.strategyStock)
      , (DeriveNewtype , Set.toList instances.strategyNewtype)
      ]

instance Semigroup SimpleInstances where
  a <> b = SimpleInstances{
        strategyNewtype = combine (.strategyNewtype)
      , strategyStock   = combine (.strategyStock)
      }
    where
      combine :: Semigroup a => (SimpleInstances -> a) -> a
      combine f = f a <> f b

instance Monoid SimpleInstances where
  mempty = SimpleInstances{
        strategyStock   = mempty
      , strategyNewtype = mempty
      }

toSimpleInstances :: SDecl -> Maybe (Hs.Name Hs.NsTypeConstr, SimpleInstances)
toSimpleInstances = \case
    DDerivingInstance (DerivingInstance DeriveStock (TApp (TClass cls) (TCon name)) _) ->
      Just (name, mempty{strategyStock = Set.singleton cls})
    DDerivingInstance (DerivingInstance DeriveNewtype (TApp (TClass cls) (TCon name)) _)->
      Just (name, mempty{strategyNewtype = Set.singleton cls})
    _otherwise
      -> Nothing

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

partitionMaybe :: (a -> Maybe b) -> [a] -> ([a], [b])
partitionMaybe f = partitionEithers . map (\x -> maybe (Left x) Right (f x))
