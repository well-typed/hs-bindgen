-- | Errors arising while zipping C ASTs before and after reparsing
--
-- Defined in its own module to avoid cyclic module dependencies.
module HsBindgen.Frontend.Pass.Zip.Error (
    ZipError(..)
  , zipErrorNotZipped
  , zipErrorNotEqual
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Type
-------------------------------------------------------------------------------}

-- | Errors arising while zipping C ASTs before and after reparsing
data ZipError =
    -- | Expected two sub-trees to be zipped, but they were not
    ZipSubTreesNotZipped
    -- | Expected two sub-trees to be equal, but they were not
  | ZipSubTreesNotEqual
    -- | Printed LHS sub-tree
  | ZipSubTreeLHS Text
    -- | Printed RHS sub-tree
  | ZipSubTreeRHS Text
  deriving stock (Show, Eq)

-- | Smart constructor for 'ZipSubTreesNotZipped'
zipErrorNotZipped :: (Show a, Show b) => a -> b -> [ZipError]
zipErrorNotZipped lhs rhs = withDebugTraces lhs rhs ZipSubTreesNotZipped

-- | Smart constructor for 'ZipSubTreesNotEqual'
zipErrorNotEqual :: (Show a, Show b) => a -> b -> [ZipError]
zipErrorNotEqual lhs rhs = withDebugTraces lhs rhs ZipSubTreesNotEqual

-- | Add debug trace messages that print the LHS and RHS sub-trees at the point
-- where zipping failed
withDebugTraces :: (Show a, Show b) => a -> b -> ZipError -> [ZipError]
withDebugTraces lhs rhs err = concat [
      [ err ]
    , [ ZipSubTreeLHS $ Text.pack $ show lhs
      , ZipSubTreeRHS $ Text.pack $ show rhs
      ]
    ]

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

instance PrettyForTrace ZipError where
  prettyForTrace = \case
    ZipSubTreesNotZipped -> PP.hsep [
          "Expected two sub-trees in the C ASTs before and after reparsing to be"
        , "zipped, but they were not."
        ]
    ZipSubTreesNotEqual -> PP.hsep [
          "Expected two sub-trees in the C ASTs before and after reparsing to be"
        , "equal, but they were not."
        ]
    ZipSubTreeLHS lhs -> "Sub-tree before reparsing:" PP.<+> PP.text lhs
    ZipSubTreeRHS rhs -> "Sub-tree after reparsing:" PP.<+> PP.text rhs

instance IsTrace Level ZipError where
  getDefaultLogLevel = \case
      -- Reparsing is best effort, so we report "regular" failures at 'Info'
      -- level rather than a higher verbosity level
      ZipSubTreesNotZipped{} -> Info
      ZipSubTreesNotEqual{}   -> Info
      -- Sub-trees are useful for debugging
      ZipSubTreeLHS{}         -> Debug
      ZipSubTreeRHS{}         -> Debug
  getSource = const HsBindgen
  getTraceId = const "zip"
