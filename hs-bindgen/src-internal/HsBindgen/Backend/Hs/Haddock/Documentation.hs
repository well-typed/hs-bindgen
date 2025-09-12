module HsBindgen.Backend.Hs.Haddock.Documentation where

import Data.Semigroup (First (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Natural (Natural)

import Clang.HighLevel.Types

import HsBindgen.Frontend.RootHeader (HashIncludeArg (..))

-- | Haddock documentation representation
--
data Comment = Comment
  { commentTitle    :: Maybe [CommentInlineContent] -- ^ Comment title
  , commentOrigin   :: Maybe Text                   -- ^ Original C name reference
  , commentLocation :: Maybe SingleLoc              -- ^ The source location of
                                                    --   the original C name reference
  , commentHeader   :: Maybe HashIncludeArg         -- ^ Header file that exports the
                                                    --   original C name reference
  , commentChildren :: [CommentBlockContent]        -- ^ Comment content
  }
  deriving (Show, Eq, Generic)

instance Semigroup Comment where
  Comment t o l h c <> Comment t' o' l' h' c' =
    Comment (t <> t')
            (fmap getFirst $ ((fmap First o) <> (fmap First o')))
            (fmap getFirst $ ((fmap First l) <> (fmap First l')))
            (fmap getFirst $ ((fmap First h) <> (fmap First h')))
            (c <> c')

instance Monoid Comment where
  mempty = Comment Nothing Nothing Nothing Nothing []

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
  deriving (Show, Eq, Generic)

-- | Special metadata that can appear in docs
--
data CommentMeta
  = Since { sinceContent :: Text } -- ^ @since 1.0
  deriving (Show, Eq, Generic)
