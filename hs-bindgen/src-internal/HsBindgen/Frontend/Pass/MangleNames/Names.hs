-- | Name bookkeeping for the name mangler
--
-- The name mangler maintains /two/ indices over the set of Haskell names it
-- mints. They answer opposite questions and are keyed differently, so they are
-- distinct structures:
--
-- * 'NameMap' is the /resolution index/: @'DeclId' -> 'Hs.Name'@. Pass 2 uses
--   it to resolve a /reference/ (it holds a target 'DeclId' and wants the
--   Haskell name that was assigned to it). It only ever contains the top-level,
--   'DeclId'-addressable names.
--
-- * 'NameRegistry' is the /collision index/: @'Hs.Name' -> ['NameOrigin']@. It
--   records minted names that may collide (top-level and derived) -- together
--   with the declaration responsible for it. Hence, we can detect when the same
--   Haskell name is produced more than once in the same namespace, whether by
--   two distinct declarations (e.g. two structs) or by a single declaration via
--   two of its members (e.g. an enum tag and one of its enumerators).
--
-- Both hold three maps, one per Haskell namespace ('Hs.NsTypeConstr',
-- 'Hs.NsConstr', 'Hs.NsVar') with types ensuring that names cannot collide
-- spuriously across namespaces.
--
-- Intended for unqualified import.
module HsBindgen.Frontend.Pass.MangleNames.Names (
    -- * Resolution index
    NameMap(..)
  , emptyNameMap
  , fromDeclIdPairs
  , lookupType
  , lookupData
  , lookupVar
    -- * Collision index
  , NameRole(..)
  , NameOrigin(..)
  , NameRegistry
  , emptyNameRegistry
  , registerName
  , registerSomeName
  , Collision(..)
  , collisions
  ) where

import Data.Foldable qualified as Foldable
import Data.Map qualified as Map

import Clang.HighLevel.Types (SingleLoc)

import HsBindgen.Frontend.Naming
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Resolution index
-------------------------------------------------------------------------------}

-- | Maps each declaration to the Haskell name assigned to it
--
-- One map per Haskell namespace.
data NameMap = NameMap {
      typeConstrs :: Map DeclId (Hs.Name Hs.NsTypeConstr)
    , dataConstrs :: Map DeclId (Hs.Name Hs.NsConstr)
    , vars        :: Map DeclId (Hs.Name Hs.NsVar)
    }
  deriving stock (Show, Eq, Generic)

emptyNameMap :: NameMap
emptyNameMap = NameMap Map.empty Map.empty Map.empty

fromDeclIdPairs :: [DeclIdPair] -> NameMap
fromDeclIdPairs = Foldable.foldl' aux emptyNameMap
  where
    aux :: NameMap -> DeclIdPair -> NameMap
    aux nameMap pair = case pair.hsName.ns of
      Hs.NsTypeConstr ->
        nameMap & #typeConstrs %~
          Map.insert pair.cName (Hs.assertNs (Proxy @Hs.NsTypeConstr) pair.hsName)
      Hs.NsConstr ->
        nameMap & #dataConstrs %~
          Map.insert pair.cName (Hs.assertNs (Proxy @Hs.NsConstr)     pair.hsName)
      Hs.NsVar ->
        nameMap & #vars %~
          Map.insert pair.cName (Hs.assertNs (Proxy @Hs.NsVar)        pair.hsName)

lookupType :: DeclId -> NameMap -> Maybe (Hs.Name Hs.NsTypeConstr)
lookupType declId nameMap = Map.lookup declId nameMap.typeConstrs

lookupData :: DeclId -> NameMap -> Maybe (Hs.Name Hs.NsConstr)
lookupData declId nameMap = Map.lookup declId nameMap.dataConstrs

lookupVar :: DeclId -> NameMap -> Maybe (Hs.Name Hs.NsVar)
lookupVar declId nameMap = Map.lookup declId nameMap.vars

{-------------------------------------------------------------------------------
  Collision index
-------------------------------------------------------------------------------}

-- | What a registered Haskell name denotes
--
-- Stored with every 'NameOrigin' to document what was minted and to give
-- collision diagnostics something to say.
data NameRole =
    -- | A top-level declaration's own name (chosen in pass 1)
    Declaration
    -- | A generated data constructor (including enum constants)
  | Constructor
    -- | A generated record field selector or newtype unwrapper
  | Field
    -- | A generated union getter or setter
  | Accessor
    -- | A generated auxiliary type constructor (FLAM or function pointer)
  | AuxType
  deriving stock (Show, Eq, Ord, Generic)

-- | A single site at which a Haskell name was minted
data NameOrigin = NameOrigin {
      -- | The declaration to blame should this name collide
      owner :: DeclId
    , loc   :: SingleLoc
    , role  :: NameRole
    }
  deriving stock (Show, Eq, Ord, Generic)

-- | Records every minted Haskell name and the sites that minted it
--
-- One map per Haskell namespace, keyed by the fully-typed name.
data NameRegistry = NameRegistry {
      typeConstrs :: Map (Hs.Name Hs.NsTypeConstr) [NameOrigin]
    , dataConstrs :: Map (Hs.Name Hs.NsConstr)     [NameOrigin]
    , vars        :: Map (Hs.Name Hs.NsVar)        [NameOrigin]
    }
  deriving stock (Show, Eq, Generic)

emptyNameRegistry :: NameRegistry
emptyNameRegistry = NameRegistry Map.empty Map.empty Map.empty

-- | Register a minted name, retaining its type-level namespace
registerName :: forall ns.
     Hs.SingNamespace ns
  => NameRole
  -> DeclId
  -> SingleLoc
  -> Hs.Name ns
  -> NameRegistry
  -> NameRegistry
registerName role owner loc name reg = case Hs.singNamespace @ns of
    Hs.SNsTypeConstr -> reg & #typeConstrs %~ Map.alter prepend name
    Hs.SNsConstr     -> reg & #dataConstrs %~ Map.alter prepend name
    Hs.SNsVar        -> reg & #vars        %~ Map.alter prepend name
  where
    origin :: NameOrigin
    origin = NameOrigin{
        owner = owner
      , loc   = loc
      , role  = role
      }

    prepend :: Maybe [NameOrigin] -> Maybe [NameOrigin]
    prepend = \case
      Nothing -> Just [origin]
      Just xs -> Just $ origin:xs

-- | Register a name whose namespace is only known at the value level
--
-- Used to seed the registry with pass 1 names (which are stored as
-- 'Hs.SomeName'). The namespace is re-asserted at the type level.
registerSomeName ::
     NameRole
  -> DeclId
  -> SingleLoc
  -> Hs.SomeName
  -> NameRegistry
  -> NameRegistry
registerSomeName role owner loc sname = case sname.ns of
    Hs.NsTypeConstr ->
      registerName role owner loc (Hs.assertNs (Proxy @Hs.NsTypeConstr) sname)
    Hs.NsConstr ->
      registerName role owner loc (Hs.assertNs (Proxy @Hs.NsConstr) sname)
    Hs.NsVar ->
      registerName role owner loc (Hs.assertNs (Proxy @Hs.NsVar) sname)

-- | A Haskell name minted by two or more distinct declarations
data Collision = Collision {
      name    :: Hs.SomeName
    , origins :: [NameOrigin]
    }
  deriving stock (Show, Eq, Generic)

-- | All collisions in the registry, across all three namespaces
--
-- A name collides when it was minted at least twice in the same namespace.
collisions :: NameRegistry -> [Collision]
collisions reg = concat [
      collisionsFor reg.typeConstrs
    , collisionsFor reg.dataConstrs
    , collisionsFor reg.vars
    ]
  where
    collisionsFor :: forall ns.
         Hs.SingNamespace ns
      => Map (Hs.Name ns) [NameOrigin]
      -> [Collision]
    collisionsFor m =
        [ Collision (Hs.demoteNs name) origins
        | (name, origins) <- Map.toList m
        , isCollision origins
        ]

    -- A name is registered once per emitted top-level Haskell entity that wants
    -- it: top-level declaration names are seeded once each (redeclarations are
    -- deduplicated by 'DeclId' before this pass), and derived names are minted
    -- once each. So two or more registrations of the same name in the same
    -- namespace always mean two or more distinct entities competing for it --
    -- be they different declarations or members of the same declaration.
    --
    -- We must not deduplicate by 'NameOrigin' here: derived names are recorded
    -- with their /owner's/ location, so e.g. an enum's data constructor and one
    -- of its enumerators share the same owner, location, and 'NameRole', and
    -- would collapse into one.
    isCollision :: [NameOrigin] -> Bool
    isCollision origins = length origins >= 2
