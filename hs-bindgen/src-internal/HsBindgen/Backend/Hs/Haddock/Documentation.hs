-- | Haddock documentation
--
-- Intended for qualified import.
--
-- > import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
module HsBindgen.Backend.Hs.Haddock.Documentation (
    -- * Definition
    Comment(..)
  , CommentInlineContent(..)
  , CommentMeta(..)
  , CommentBlockContent(..)
  , HeaderLevel(..)
  , ListType(..)
    -- * Construction helpers
  , title
  , simple
  , uniqueSymbol
  , paragraph
  , monospace
  ) where

import Data.Semigroup (First (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Natural (Natural)

import Clang.HighLevel.Types

import HsBindgen.Backend.Hs.AST.Type (HsType)
import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Frontend.AST.Decl qualified as C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Haddock documentation representation
--
data Comment = Comment {
    -- | Comment title
    commentTitle :: Maybe [CommentInlineContent]

    -- | Original C name reference
  , commentOrigin :: Maybe Text

    -- | The source location of the original C name reference
  , commentLocation :: Maybe SingleLoc

    -- | Header information
  , commentHeaderInfo :: Maybe C.HeaderInfo

    -- | Unique symbol used to generate this binding
  , commentUnique :: Maybe UniqueSymbol

    -- | | Comment content
  , commentChildren :: [CommentBlockContent]
  }
  deriving (Show, Eq, Generic)

instance Semigroup Comment where
  a <> b = Comment {
        commentTitle      = combine commentTitle      (<>)
      , commentOrigin     = combine commentOrigin     first
      , commentLocation   = combine commentLocation   first
      , commentHeaderInfo = combine commentHeaderInfo first
      , commentUnique     = combine commentUnique     first
      , commentChildren   = combine commentChildren   (<>)
      }
    where
      combine :: (Comment -> a) -> (a -> a -> a) -> a
      combine f op = f a `op` f b

      first :: Maybe a -> Maybe a -> Maybe a
      first x y = fmap getFirst (fmap First x <> fmap First y)

instance Monoid Comment where
  mempty = Comment {
        commentTitle      = Nothing
      , commentOrigin     = Nothing
      , commentLocation   = Nothing
      , commentHeaderInfo = Nothing
      , commentUnique     = Nothing
      , commentChildren   = []
      }

-- | Block-level Haddock content
--
data CommentBlockContent
  = Paragraph
      { paragraphContent      :: [CommentInlineContent] -- ^ Paragraph content
      }
  | CodeBlock
      { codeBlockLines        :: [Text] -- ^ @ ... @
      }
  | Verbatim
      { verbatimContent       :: Text -- ^ > code
      }
  | Example
      { exampleContent        :: Text -- ^ >>> ... (example)
      }
  | Property
      { propertyContent       :: Text -- ^ prop> ... (properties)
      }
  | ListItem
      { listItemType          :: ListType
      , listItemContent       :: [CommentBlockContent]
      }
  | DefinitionList                                     -- ^ [term]: definition
      { definitionListTerm    :: CommentInlineContent  -- ^ [term]
      , definitionListContent :: [CommentBlockContent] -- ^ definition
      }
  | Header
      { headerLevel           :: HeaderLevel
      , headerContent         :: [CommentInlineContent]
      }
  deriving (Show, Eq, Generic)

data ListType
  = BulletList           -- ^ * item
  | NumberedList Natural -- ^ 1. item
  deriving (Show, Eq, Generic)

-- | Haddock levels only go up to 6. Extra '=' are treated as belonging to the
-- text of the heading.
--
data HeaderLevel
  = Level1 -- ^ =
  | Level2 -- ^ ==
  | Level3 -- ^ ===
  | Level4 -- ^ ====
  | Level5 -- ^ =====
  | Level6 -- ^ ======
  deriving (Show, Enum, Eq, Generic)

-- | Inline content
--
data CommentInlineContent
  = TextContent
      { textContent       :: Text
      }
  | Monospace
      { monospaceContent  :: [CommentInlineContent] -- ^ @ ... @
      }
  | Emph
      { emphContent       :: [CommentInlineContent] -- ^ / ... /
      }
  | Bold
      { boldContent       :: [CommentInlineContent] -- ^ __ ... __
      }
  | Module
      { moduleContent     :: Text -- ^ "Module"
      }
  | Identifier
      { identifierContent :: Text -- ^ 'identifier'
      }
  | Type
      { typeContent       :: Text -- ^ t'Type'
      }
  | Link                                            -- ^ [label](url)
      { linkLabel         :: [CommentInlineContent] -- ^ link label
      , linkURL           :: Text                   -- ^ link url
      }
  | URL
      { urlContent        :: Text -- ^ <url>
      }
  | Anchor
      { anchorContent     :: Text -- ^ #anchor#
      }
  | Math
      { mathContent       :: [Text] -- ^ \( ... \) or \[ ... \]
      }
  | Metadata
      { metadataContent   :: CommentMeta
      }
  | TypeSignature
      { typeSignature     :: HsType
      }
    -- ^ This constructor allows one to render a 'HsType'. For example, when
    -- generating FFI "wrapper" stubs it could be useful to take a 'HsFun'
    -- type and include it in the Haddocks, however one only has access to
    -- the 'HsType' pretty printer at rendering time.
  deriving (Show, Eq, Generic)

-- | Special metadata that can appear in docs
--
data CommentMeta
  = Since { sinceContent :: Text } -- ^ @since 1.0
  deriving (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Construction helpers
-------------------------------------------------------------------------------}

-- | Title only
title :: [CommentInlineContent] -> Comment
title content = mempty{commentTitle = Just content }

-- | Content only
simple :: [CommentBlockContent] -> Comment
simple content = mempty{commentChildren = content}

-- | Just record a unique symbol
uniqueSymbol :: UniqueSymbol -> Comment
uniqueSymbol unique = mempty{commentUnique = Just unique}

-- | Single paragraph comment
paragraph :: [CommentInlineContent] -> Comment
paragraph = simple . (:[]) . Paragraph

monospace :: Text -> CommentInlineContent
monospace = Monospace . (:[]) . TextContent
