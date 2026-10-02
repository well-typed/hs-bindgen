-- | Paths of declaration locations
--
-- This module should only be used within the @HsBindgen.IR@ hierarchy.  From
-- outside the @HsBindgen.IR@ hierarchy, "HsBindgen.IR.C" should be used.
--
-- Within @HsBindgen.IR@, all modules aside from "HsBindgen.IR.C" should import
-- this module qualified for consistency.
--
-- > import HsBindgen.IR.C.DeclPath qualified as C
module HsBindgen.IR.C.DeclPath (
    DeclPath(..)
  , declPathRealPath
  , rootHeaderName
  ) where

import Clang.HighLevel (ShowFile (..))
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Where a declaration is located
--
-- Only headers are files on disk. The root header is an unsaved file, and Clang
-- writes each @-D@ option as a @#define@ into its predefines buffer, which has
-- no file at all; neither has a 'RealPath'.
--
-- The constructor order follows the order in which Clang processes the
-- sources.
data DeclPath =
    -- | A @-D@ Clang option
    OnCommandLine
    -- | The root header (see "HsBindgen.Frontend.RootHeader")
  | InRootHeader
    -- | A header
  | InHeader RealPath
  deriving stock (Eq, Ord, Generic)

instance Show DeclPath where
  show = renderDeclPath

instance Show (SingleLoc DeclPath) where
  show = show . HighLevel.prettySingleLoc renderDeclPath ShowFile

declPathRealPath :: DeclPath -> Maybe RealPath
declPathRealPath = \case
    OnCommandLine -> Nothing
    InRootHeader  -> Nothing
    InHeader path -> Just path

{-------------------------------------------------------------------------------
  Names
-------------------------------------------------------------------------------}

-- | Name of the root header @UnsavedFile@
rootHeaderName :: SourcePath
rootHeaderName = SourcePath "hs-bindgen-root.h"

renderDeclPath :: DeclPath -> String
renderDeclPath = \case
    OnCommandLine -> "<command line>"
    InRootHeader  -> "<root header>"
    InHeader path -> getRealPath path
