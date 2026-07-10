module HsBindgen.Frontend.Pass.MangleNames.Error (
    -- * Unifying error
    MangleNamesError (..)
    -- * Per-traversal errors
  , MangleNamesCreationError (..)
  , MangleNamesCollisionError (..)
  , MangleNamesResolutionError (..)
  ) where

import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types (SingleLoc)

import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Unifying error

  The three traversals each produce their own error type. Selection, however,
  consumes a single mangle-names error: it is stored in the 'DeclIndex' as the
  reason a declaration is unusable, and threaded through the 'Select' pass.
  'MangleNamesError' unifies the three at that boundary.
-------------------------------------------------------------------------------}

data MangleNamesError =
    MangleNamesCreationError   MangleNamesCreationError
  | MangleNamesCollisionError  MangleNamesCollisionError
  | MangleNamesResolutionError MangleNamesResolutionError
  deriving stock (Show, Eq, Ord)

instance PrettyForTrace MangleNamesError where
  prettyForTrace = \case
      MangleNamesCreationError   e -> prettyForTrace e
      MangleNamesCollisionError  e -> prettyForTrace e
      MangleNamesResolutionError e -> prettyForTrace e

instance IsTrace Level MangleNamesError where
  getDefaultLogLevel = \case
    MangleNamesCreationError CreateNamesSquashMustBeTypeConstr{} -> Bug
    MangleNamesCreationError{}                                   -> Warning
    MangleNamesCollisionError{}                                  -> Warning
    MangleNamesResolutionError{}                                 -> Warning
  getSource  = const HsBindgen
  getTraceId = const "mangle-names"

{-------------------------------------------------------------------------------
  Traversal 1: create names
-------------------------------------------------------------------------------}

-- | Errors of the "create names" traversal
data MangleNamesCreationError =
    -- | A C name could not be mangled into a valid Haskell name.
    CreateNamesCouldNotMangle Text
    -- | Bug: squash analysis produced a 'Squash' result for a declaration
    -- whose namespace is not 'Hs.NsTypeConstr'. Squashes must always be types.
  | CreateNamesSquashMustBeTypeConstr Hs.Namespace
  deriving stock (Show, Eq, Ord)

instance PrettyForTrace MangleNamesCreationError where
  prettyForTrace = \case
      CreateNamesCouldNotMangle name -> PP.hsep [
          "Could not mangle C name:"
        , PP.text name
        ]
      CreateNamesSquashMustBeTypeConstr ns -> PP.hsep [
          "Bug: squash for non-type-constructor namespace:"
        , prettyForTrace ns
        ]

{-------------------------------------------------------------------------------
  Traversal 2: detect clashes
-------------------------------------------------------------------------------}

-- | Errors of the "detect clashes" traversal
data MangleNamesCollisionError =
    -- | Two or more declarations mangle to the same Haskell name.
    DetectClashesCollision Hs.SomeName [C.WithLocationInfo C.DeclId]
    -- | Two or more fields in the same struct mangle to the same Haskell name.
    --
    -- Only fires under 'OmitFieldPrefixes'; under 'AddFieldPrefixes' mangling
    -- is injective within a record.
  | DetectClashesDuplicateFieldName Hs.SomeName [SingleLoc]
  deriving stock (Show, Eq, Ord)

instance PrettyForTrace MangleNamesCollisionError where
  prettyForTrace = \case
      DetectClashesCollision x xs ->
        let intro = PP.hcat [
                "Colliding definitions for Haskell name "
              , prettyForTrace x
              , ":"
              ]
        in  PP.hang intro 2 $ PP.vcat $ map prettyForTrace xs
      DetectClashesDuplicateFieldName x locs ->
        let intro = PP.hcat [
                "Duplicate record field name "
              , prettyForTrace x
              , ":"
              ]
        in  PP.hang intro 2 $ PP.vcat $ map PP.show locs

{-------------------------------------------------------------------------------
  Traversal 3: resolve names
-------------------------------------------------------------------------------}

-- | Errors of the "resolve names" traversal
data MangleNamesResolutionError =
    -- | A declaration references another declaration that was not mangled
    -- (because it failed to mangle, or was dropped due to a collision), leaving
    -- a dangling reference.
    ResolveNamesUnderlyingDeclNotMangled C.DeclId (NonEmpty Hs.Namespace)
  deriving stock (Show, Eq, Ord)

instance PrettyForTrace MangleNamesResolutionError where
  prettyForTrace = \case
      ResolveNamesUnderlyingDeclNotMangled declId namespaces -> PP.hsep [
          "Underlying"
        , PP.hsep $ List.intersperse "or" $
            map prettyForTrace $ NonEmpty.toList namespaces
        , "not mangled:"
        , prettyForTrace declId
        ]
