-- | Declaration index
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
-- > import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
module HsBindgen.Frontend.Analysis.DeclIndex (
    DeclIndex -- opaque
    -- * Construction
  , DeclIndexError(..)
  , fromDecls
    -- * Query
  , lookup
  , (!)
  ) where

import Prelude hiding (lookup)

import Control.Monad.State
import Data.Function
import Data.Map.Strict qualified as Map

import Clang.HighLevel.Types
import HsBindgen.Errors
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Type.DeclId
import HsBindgen.Imports
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint (hcat, showToCtxDoc)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Index of all declarations we have parsed
--
-- This excludes declarations that were not excluded by the selection predicate.
newtype DeclIndex = Wrap {
      unwrap :: Map QualDeclId (C.Decl Parse)
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Construction state (internal type)
data PartialIndex = PartialIndex{
      index  :: !(Map QualDeclId (C.Decl Parse))
    , errors :: !(Map QualDeclId DeclIndexError)
    }

fromDecls :: [C.Decl Parse] -> (DeclIndex, [DeclIndexError])
fromDecls decls =
      fromPartialIndex
    . flip execState (PartialIndex Map.empty Map.empty)
    $ mapM_ aux decls
  where
    fromPartialIndex :: PartialIndex -> (DeclIndex, [DeclIndexError])
    fromPartialIndex (PartialIndex declIndex errors) = (
        Wrap declIndex
      , Map.elems errors
      )

    aux :: C.Decl Parse -> State PartialIndex ()
    aux decl = modify' $ \oldState@PartialIndex{index, errors} ->
        if Map.member qid errors then
          -- Ignore further definitions of the same ID after an error
          oldState
        else
          let (index', mErr) = flip runState Nothing $
                 Map.alterF (insert decl) qid index
          in PartialIndex{
              index  = index'
            , errors = case mErr of
                         Nothing -> errors
                         Just e  -> Map.insert qid e errors
            }
     where
       qid :: QualDeclId
       qid = declQualDeclId decl

    insert ::
         C.Decl Parse
      -> Maybe (C.Decl Parse)
      -> State (Maybe DeclIndexError) (Maybe (C.Decl Parse))
    insert new mOld = state $ \_ ->
        case mOld of
          Nothing ->
              -- The normal case: no previous declaration exists
              success new

          Just old
            | sameDefinition (C.declKind new) (C.declKind old) ->
                -- Redeclaration but with the same definition. This can happen,
                -- for example for opaque structs. We stick with the first.
                success old

            | otherwise ->
                -- Redeclaration with a /different/ value. This is only legal
                -- for macros; for other kinds of declarations, clang will have
                -- reported an error already.
                failure $ Redeclaration{
                    redeclarationId  = declQualDeclId new
                  , redeclarationOld = C.declLoc $ C.declInfo old
                  , redeclarationNew = C.declLoc $ C.declInfo new
                  }
     where

       -- No errors; set (or replace) value in the map
       success :: C.Decl Parse -> (Maybe (C.Decl Parse), Maybe DeclIndexError)
       success decl = (Just decl, Nothing)

       -- In case of an error, /remove/ the value from the map
       failure :: DeclIndexError -> (Maybe (C.Decl Parse), Maybe DeclIndexError)
       failure err = (Nothing, Just err)

sameDefinition :: C.DeclKind Parse -> C.DeclKind Parse -> Bool
sameDefinition a b =
    case (a, b) of
      (C.DeclMacro macroA, C.DeclMacro macroB) ->
        sameMacro macroA macroB
      _otherwise ->
        a == b

sameMacro :: UnparsedMacro -> UnparsedMacro -> Bool
sameMacro = (==) `on` (map tokenSpelling . unparsedTokens)

{-------------------------------------------------------------------------------
  Construction errors
-------------------------------------------------------------------------------}

data DeclIndexError =
    Redeclaration {
        redeclarationId  :: QualDeclId
      , redeclarationOld :: SingleLoc
      , redeclarationNew :: SingleLoc
      }
  deriving stock (Show, Eq)

instance PrettyForTrace DeclIndexError where
  prettyForTrace Redeclaration{..} = hcat [
        prettyForTrace redeclarationId
      , " declared at "
      , showToCtxDoc redeclarationOld
      , " was redeclared at "
      , showToCtxDoc redeclarationNew
      , ". No binding generated."
      ]

instance HasDefaultLogLevel DeclIndexError where
  getDefaultLogLevel = \case
      -- Redeclarations can only happen for macros, so we issue a warning,
      -- rather than an error.
      Redeclaration{} -> Warning

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

lookup :: QualDeclId -> DeclIndex -> Maybe (C.Decl Parse)
lookup qid = Map.lookup qid . unwrap

(!) :: HasCallStack => DeclIndex -> QualDeclId -> C.Decl Parse
(!) declIndex qid =
    fromMaybe (panicPure $ "Unknown key: " ++ show qid) $
       lookup qid declIndex
