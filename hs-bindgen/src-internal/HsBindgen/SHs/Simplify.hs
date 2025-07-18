-- | Simplification pass on the "HsBindgen.SHs.AST" representation
module HsBindgen.SHs.Simplify (simplifySHs) where

import Data.Map.Strict qualified as Map
import Data.Set        qualified as Set

import HsBindgen.Hs.AST.Strategy
import HsBindgen.Imports
import HsBindgen.Language.Haskell
import HsBindgen.SHs.AST
import Data.Either (partitionEithers)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

simplifySHs :: [SDecl] -> [SDecl]
simplifySHs decls =
    let (decls', simpleInstances) =
          second (Map.fromListWith (<>)) $
            partitionMaybe toSimpleInstances decls
    in reconstruct simpleInstances decls'

reconstruct ::
     Map (HsName NsTypeConstr) SimpleInstances
  -> [SDecl] -> [SDecl]
reconstruct simpleInstances = map aux
  where
    aux :: SDecl -> SDecl
    aux = \case
        DRecord x@Record{dataType, dataDeriv} ->
          DRecord x{dataDeriv = dataDeriv ++ instancesFor dataType}
        DNewtype x@Newtype{newtypeName, newtypeDeriv} ->
          DNewtype x{newtypeDeriv = newtypeDeriv ++ instancesFor newtypeName}
        otherDecl ->
          otherDecl

    instancesFor :: HsName 'NsTypeConstr -> [(Strategy ClosedType, [Global])]
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
      strategyStock   :: Set Global
    , strategyNewtype :: Set Global
    }
  deriving stock (Show)

fromSimpleInstances :: SimpleInstances -> [(Strategy ClosedType, [Global])]
fromSimpleInstances SimpleInstances{..} =
    filter (not . null . snd) $ [
        (DeriveStock   , Set.toList strategyStock)
      , (DeriveNewtype , Set.toList strategyNewtype)
      ]

instance Semigroup SimpleInstances where
  a <> b = SimpleInstances{
        strategyNewtype = combine strategyNewtype
      , strategyStock   = combine strategyStock
      }
    where
      combine :: Semigroup a => (SimpleInstances -> a) -> a
      combine f = f a <> f b

instance Monoid SimpleInstances where
  mempty = SimpleInstances{
        strategyStock   = mempty
      , strategyNewtype = mempty
      }

toSimpleInstances :: SDecl -> Maybe (HsName NsTypeConstr, SimpleInstances)
toSimpleInstances = \case
    DDerivingInstance DeriveStock (TApp (TGlobal cls) (TCon name)) ->
      Just (name, mempty{strategyStock = Set.singleton cls})
    DDerivingInstance DeriveNewtype (TApp (TGlobal cls) (TCon name)) ->
      Just (name, mempty{strategyNewtype = Set.singleton cls})
    _otherwise
      -> Nothing

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

partitionMaybe :: (a -> Maybe b) -> [a] -> ([a], [b])
partitionMaybe f = partitionEithers . map (\x -> maybe (Left x) Right (f x))
