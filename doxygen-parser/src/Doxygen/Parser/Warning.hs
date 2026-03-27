-- | Structured warnings for unsupported or degraded Doxygen content
--
-- When the parser encounters Doxygen XML elements it cannot fully represent
-- in the typed AST, it emits a 'Warning' describing what happened and how
-- the content was degraded.
--
module Doxygen.Parser.Warning (
    Warning(..)
  , Context(..)
  , Degradation(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | A structured warning about unsupported or degraded content
data Warning = Warning {
    element     :: Text
    -- ^ The XML element or feature that triggered the warning
  , context     :: Context
    -- ^ Where in the AST the element appeared
  , degradation :: Degradation
    -- ^ How the content was degraded
  , explanation :: Text
    -- ^ Human-readable description of what happened
  }
  deriving stock (Show, Eq, Generic)

-- | Where in the AST hierarchy the unsupported element appeared
data Context
  = BlockLevel
    -- ^ Inside a block-level context (e.g., child of @\<para\>@ or
    -- @\<detaileddescription\>@)
  | InlineLevel
    -- ^ Inside an inline context (e.g., child of @\<bold\>@ or inline
    -- content within @\<para\>@)
  | UnknownSectKind
    -- ^ An unrecognised @kind@ attribute on @\<simplesect\>@
  | StructureLevel Text
    -- ^ Inside a structural element.  The 'Text' identifies the parent
    -- (e.g. @\"compounddef\"@, @\"sectiondef\"@, @\"memberdef\"@).
  deriving stock (Show, Eq, Generic)

-- | How the unsupported content was handled
data Degradation
  = Omitted
    -- ^ Content was entirely dropped from the AST
  | DegradedToText
    -- ^ Formatting was lost; content preserved as plain text
  | DefaultedTo Text
    -- ^ An unknown value was replaced with a default
  deriving stock (Show, Eq, Generic)
