-- | Declaration index
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
-- > import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
module HsBindgen.Frontend.Analysis.DeclIndex (
    DeclIndex -- opaque
    -- * Construction
  , fromDecls
    -- * Query
  , lookup
  , (!)
  ) where

import Prelude hiding (lookup)

import Data.Map.Strict qualified as Map

import HsBindgen.Errors
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Index of all declarations we have parsed
--
-- This excludes declarations that were not excluded by the selection predicate.
newtype DeclIndex = Wrap {
      unwrap :: Map (C.QualId Parse) (C.Decl Parse)
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromDecls :: HasCallStack => [C.Decl Parse] -> DeclIndex
fromDecls decls = Wrap $
    Map.fromListWith aux $ map (\d -> (C.declQualId d, d)) decls
  where
    -- Some declarations can be repeated, but if so, they must be essentially
    -- the same. For example, this is valid C, which declares the same struct
    -- "foo" twice:
    --
    -- > struct foo;
    -- > struct foo;
    aux :: C.Decl Parse -> C.Decl Parse -> C.Decl Parse
    aux new old
      | C.declKind old == C.declKind new
      = old

      | otherwise
      = panicPure $ "duplicate declaration for " ++ show (C.declQualId new)

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

lookup :: C.QualId Parse -> DeclIndex -> Maybe (C.Decl Parse)
lookup uid = Map.lookup uid . unwrap

(!) :: HasCallStack => DeclIndex -> C.QualId Parse -> C.Decl Parse
(!) ud uid =
    fromMaybe (panicPure $ "Unknown key: " ++ show uid) $
       lookup uid ud
