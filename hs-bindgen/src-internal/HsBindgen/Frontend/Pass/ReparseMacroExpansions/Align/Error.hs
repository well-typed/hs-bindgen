-- | Errors arising while aligning C ASTs before and after reparsing
--
-- Defined in its own module to avoid cyclic module dependencies.
--
-- Intended to be imported qualified:
--
-- > import HsBindgen.Frontend.Pass.ReparseMacroExpansions.Align.Error qualified as Align
module HsBindgen.Frontend.Pass.ReparseMacroExpansions.Align.Error (
    Error(..)
  , errMacroRefBefore
  , errNotAligned
  , errNotEqual
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Frontend.AST.Type (MacroRef, Ref (name))
import HsBindgen.Frontend.Naming (CDeclName (text), DeclId (name))
import HsBindgen.Frontend.Pass (IsPass (MacroId))
import HsBindgen.Util.Tracer (IsTrace (..), Level (Bug, Debug, Info),
                              PrettyForTrace (..), Source (HsBindgen))

{-------------------------------------------------------------------------------
  Type
-------------------------------------------------------------------------------}

-- | Errors arising while aligning C ASTs before and after reparsing
data Error =
    -- | Encountered unexpected reference to a macro in a C type before
    -- reparsing
    --
    -- References to a macro type should only exist /after/ reparsing
    MacroRefExistsBeforeReparsing
      -- | Name of the referenced macro
      Text
    -- | Expected two sub-trees to be aligned, but they were not
  | SubTreesNotAligned
    -- | Expected two sub-trees to be equal, but they were not
  | SubTreesNotEqual
    -- | Printed LHS sub-tree
  | SubTreeLHS Text
    -- | Printed RHS sub-tree
  | SubTreeRHS  Text
  deriving stock (Show, Eq)

-- | Smart constructor for 'MacroRefExistsBeforeReparsing'
errMacroRefBefore ::
     (MacroId p ~ DeclId, Show a)
  => MacroRef p -> a -> a -> [Error]
errMacroRefBefore ref lhs rhs =
    withDebugTraces lhs rhs $ MacroRefExistsBeforeReparsing ref.name.name.text

-- | Smart constructor for 'SubTreesNotAligned'
errNotAligned :: Show a => a -> a -> [Error]
errNotAligned lhs rhs = withDebugTraces lhs rhs SubTreesNotAligned

-- | Smart constructor for 'SubTreesNotEqual'
errNotEqual :: Show a => a -> a -> [Error]
errNotEqual lhs rhs = withDebugTraces lhs rhs SubTreesNotEqual

-- | Add debug trace messages that print the LHS and RHS sub-trees at the point
-- where alignment failed
withDebugTraces :: (Show a, Show b) => a -> b -> Error -> [Error]
withDebugTraces lhs rhs err = concat [
      [ err ]
    , [ SubTreeLHS $ Text.pack $ show lhs
      , SubTreeRHS $ Text.pack $ show rhs
      ]
    ]

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

instance PrettyForTrace Error where
  prettyForTrace = \case
    MacroRefExistsBeforeReparsing macroName -> PP.hsep [
          "Encountered unexpected reference to macro"
        , PP.text macroName
        , "in a C type before reparsing."
        ]
    SubTreesNotAligned -> PP.hsep [
          "Expected two sub-trees in the C ASTs before and after reparsing to be"
        , "aligned, but they were not."
        ]
    SubTreesNotEqual -> PP.hsep [
          "Expected two sub-trees in the C ASTs before and after reparsing to be"
        , "equal, but they were not."
        ]
    SubTreeLHS lhs -> "Sub-tree before reparsing:" PP.<+> PP.text lhs
    SubTreeRHS rhs -> "Sub-tree after reparsing:" PP.<+> PP.text rhs

instance IsTrace Level Error where
  getDefaultLogLevel = \case
      -- This is a genuine bug in our implementation, because macro references
      -- should not exist in our C AST before reparsing
      MacroRefExistsBeforeReparsing{} -> Bug
      -- Reparsing is best effort, so we report "regular" failures at 'Info'
      -- level rather than a higher verbosity level
      SubTreesNotAligned{}            -> Info
      SubTreesNotEqual{}              -> Info
      -- Sub-trees are useful for debugging
      SubTreeLHS{} -> Debug
      SubTreeRHS{} -> Debug
  getSource = const HsBindgen
  getTraceId = const "reparse-align"
