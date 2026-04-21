-- | Doxygen comment types parameterized by cross-reference type
--
-- These types represent structured Doxygen comments parsed from XML output.
-- They are parameterized by @ref@ so that cross-references can be threaded
-- through consumer-specific pipelines and resolved later.
--
-- The structure closely mirrors Doxygen's XML schema:
--
-- * 'Comment' splits brief and detailed descriptions
-- * 'Block' represents block-level content (paragraphs, lists, code, etc.)
-- * 'Inline' represents inline formatting and cross-references
-- * 'Param' represents documented parameters with direction
--
-- Intended for qualified import:
--
-- @
-- import Doxygen.Parser.Types qualified as Doxy
-- @
--
module Doxygen.Parser.Types (
    -- * Comment types
    Comment(..)
  , Block(..)
  , Inline(..)
  , Param(..)
    -- * Enumerations
  , ParamListKind(..)
  , ParamDirection(..)
  , SimpleSectKind(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

{-------------------------------------------------------------------------------
  Comment types
-------------------------------------------------------------------------------}

-- | A Doxygen comment with brief and detailed sections
--
-- The @ref@ parameter is the type used for cross-references ('Ref').
-- The parser produces @Comment Text@ where references are raw C names.
-- Consumers can 'fmap' to resolve these to their own identifier types.
--
-- Corresponds to the @\<briefdescription\>@ and @\<detaileddescription\>@
-- elements in Doxygen XML output.
--
data Comment ref = Comment {
    brief    :: [Inline ref]
  , detailed :: [Block ref]
  }
  deriving stock (Functor, Foldable, Traversable, Show, Eq, Generic)

-- | Block-level content in a Doxygen comment
--
data Block ref
  = Paragraph [Inline ref]
    -- ^ A paragraph: @\<para\>@ element
  | ParamList ParamListKind [Param ref]
    -- ^ Parameter or return value list: @\<parameterlist\>@
  | SimpleSect SimpleSectKind [Block ref]
    -- ^ Special section: @\<simplesect kind=\"...\"\>@
  | CodeBlock [Text]
    -- ^ Code block: @\<programlisting\>@
  | ItemizedList [[Block ref]]
    -- ^ Bullet list: @\<itemizedlist\>@. Each element is one list item.
  | OrderedList [[Block ref]]
    -- ^ Numbered list: @\<orderedlist\>@. Each element is one list item.
  | XRefSect Text [Block ref]
    -- ^ Cross-reference section: @\<xrefsect\>@
    --
    -- Used for @\@deprecated@ and similar. First field is the title
    -- (e.g., \"Deprecated\"), second is the description.
  | Tag Text [Block ref]
    -- ^ Unsupported or unrecognised XML\/HTML tag.
    --
    -- Used for tags that do not have a dedicated constructor (e.g., HTML
    -- @\<table\>@).  The 'Text' is the element name; the @[Block ref]@
    -- are the recursively-parsed children, so that content inside
    -- unsupported tags is still preserved.
  deriving stock (Functor, Foldable, Traversable, Show, Eq, Generic)

-- | Inline content in a Doxygen comment
data Inline ref
  = Text Text
    -- ^ Plain text
  | Bold [Inline ref]
    -- ^ Bold text: @\<bold\>@ element (from @\@b@ or @\<b\>@)
  | Emph [Inline ref]
    -- ^ Emphasized text: @\<emphasis\>@ element (from @\@e@, @\@a@, or @\<i\>@)
  | Mono [Inline ref]
    -- ^ Monospace text: @\<computeroutput\>@ element (from @\@c@, @\@p@, or @\<code\>@)
  | Ref ref Text
    -- ^ Cross-reference: @\<ref\>@ element
    --
    -- First field is the reference (consumer-specific type).
    -- Second field is the display text (the C name as shown in the comment).
  | Anchor Text
    -- ^ Anchor: @\<anchor\>@ element
  | Link [Inline ref] Text
    -- ^ Hyperlink: @\<ulink\>@ element. First field is the label, second is the URL.
  deriving stock (Functor, Foldable, Traversable, Show, Eq, Generic)

-- | A documented parameter
--
data Param ref = Param {
    paramName      :: Text
  , paramDirection :: Maybe ParamDirection
  , paramDesc      :: [Block ref]
  }
  deriving stock (Functor, Foldable, Traversable, Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Enumerations
-------------------------------------------------------------------------------}

-- | Kind of parameter list
--
data ParamListKind
  = ParamListParam
    -- ^ @\@param@ documentation
  | ParamListRetVal
    -- ^ @\@retval@ documentation
  deriving stock (Show, Eq, Generic)

-- | Parameter passing direction
--
data ParamDirection
  = DirIn
    -- ^ Input parameter: @\@param[in]@
  | DirOut
    -- ^ Output parameter: @\@param[out]@
  | DirInOut
    -- ^ Input/output parameter: @\@param[in,out]@
  deriving stock (Show, Eq, Generic)

-- | Kind of simple section
--
data SimpleSectKind
  = SSReturn
    -- ^ @\@return@ / @\@returns@ / @\@result@
  | SSWarning
    -- ^ @\@warning@
  | SSNote
    -- ^ @\@note@
  | SSSee
    -- ^ @\@see@ / @\@sa@
  | SSSince
    -- ^ @\@since@
  | SSVersion
    -- ^ @\@version@
  | SSPre
    -- ^ @\@pre@
  | SSPost
    -- ^ @\@post@
  | SSPar Text
    -- ^ @\@par Title:@ — the 'Text' is the paragraph title
  | SSDeprecated
    -- ^ @\@deprecated@
  | SSRemark
    -- ^ @\@remark@ / @\@remarks@
  | SSAttention
    -- ^ @\@attention@
  | SSTodo
    -- ^ @\@todo@
  | SSInvariant
    -- ^ @\@invariant@
  | SSAuthor
    -- ^ @\@author@
  | SSDate
    -- ^ @\@date@
  deriving stock (Show, Eq, Generic)
