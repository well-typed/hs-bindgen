module HsBindgen.Hs.NameMangler.DSL.Overrides (
    Overrides(..)
    -- * Construction
  , overridesNone
  , overridesMap
    -- * Using overrides
  , useOverride
  ) where

import Control.Exception
import Data.Map qualified as Map

import HsBindgen.C.AST
import HsBindgen.Errors
import HsBindgen.Hs.AST.Name
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Override translations of Haskell names
--
-- Since not all generated Haskell names have corresponding C names, overriding
-- is primarily done based on the generated Haskell name.  In some cases the C
-- name can be used for disambiguation, if more than one C name is being
-- translated to the same Haskell name.
data Overrides = Overrides {
      override :: forall ns.
           SingNamespace ns
        => [CName]
           -- ^ C names
           --
           -- This will be a singleton list for types declared at the top level,
           -- and a longer list for nested types.
        -> Maybe (HsName ns)
           -- ^ Haskell name
           --
           -- This will be 'Nothing' only if we fail to construct the Haskell
           -- name altogether. For example, this can happen if a C type is
           -- called @_123@, and we are using the 'mkHsNameDropInvalid'
           -- policy; in this case, /all/ characters are invalid, and so we'd
           -- end up with nothing.
        -> Maybe (HsName ns)
    }

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Do not override any translations
overridesNone :: Overrides
overridesNone = Overrides $ \_cname _name -> Nothing

-- | Override translations of Haskell names using a map
overridesMap ::
     Map Namespace (Map (Maybe Text) (Map [CName] Text))
  -> Overrides
overridesMap overrideMap = Overrides aux
  where
    aux :: forall ns.
         SingNamespace ns
      => [CName] -> Maybe (HsName ns) -> Maybe (HsName ns)
    aux cnames name = do
        nsMap <- Map.lookup (namespaceOf (singNamespace @ns)) overrideMap
        nMap  <- Map.lookup (getHsName <$> name) nsMap
        HsName <$> Map.lookup cnames nMap

{-------------------------------------------------------------------------------
  Using overrides
-------------------------------------------------------------------------------}

useOverride ::
     [CName]            -- ^ Input to name generation
  -> Maybe (HsName ns)  -- ^ Generated name (unless failed)
  -> Maybe (HsName ns)  -- ^ Override (if any)
  -> HsName ns
useOverride _     _           (Just name) = name
useOverride _     (Just name) _           = name
useOverride input Nothing     Nothing     = throw $ RequireOverride input

data RequireOverride = RequireOverride [CName]
  deriving stock (Show)

instance Exception RequireOverride where
  toException   = hsBindgenExceptionToException
  fromException = hsBindgenExceptionFromException

  displayException (RequireOverride input) = concat [
        "Require name override for "
      , show input
      ]
