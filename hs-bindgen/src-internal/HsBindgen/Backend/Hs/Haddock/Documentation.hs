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

import Data.Semigroup qualified as Semigroup

import Clang.HighLevel.Types

import HsBindgen.Backend.Hs.AST.Type (HsType)
import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Haddock documentation representation
--
data Comment = Comment {
    -- | Comment title
    title :: Maybe [CommentInlineContent]

    -- | Original C name reference
  , origin :: Maybe Text

    -- | The source location of the original C name reference
  , location :: Maybe SingleLoc

    -- | Header information
  , headerInfo :: Maybe C.HeaderInfo

    -- | Unique symbol used to generate this binding
  , unique :: Maybe UniqueSymbol

    -- | | Comment content
  , children :: [CommentBlockContent]
  }
  deriving (Show, Eq, Generic)

instance Semigroup Comment where
  a <> b = Comment {
        title      = combine (.title)      (<>)
      , origin     = combine (.origin)     getFirst
      , location   = combine (.location)   getFirst
      , headerInfo = combine (.headerInfo) getFirst
      , unique     = combine (.unique)     getFirst
      , children   = combine (.children)   (<>)
      }
    where
      combine :: (Comment -> a) -> (a -> a -> a) -> a
      combine f op = f a `op` f b

      getFirst :: Maybe a -> Maybe a -> Maybe a
      getFirst x y =
          Semigroup.getFirst <$>
            (Semigroup.First <$> x) <> (Semigroup.First <$> y)

instance Monoid Comment where
  mempty = Comment {
        title      = Nothing
      , origin     = Nothing
      , location   = Nothing
      , headerInfo = Nothing
      , unique     = Nothing
      , children   = []
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
title content = mempty & #title .~ Just content

-- | Content only
simple :: [CommentBlockContent] -> Comment
simple content = mempty & #children .~ content

-- | Just record a unique symbol
uniqueSymbol :: UniqueSymbol -> Comment
uniqueSymbol unique = mempty & #unique .~ Just unique

-- | Single paragraph comment
paragraph :: [CommentInlineContent] -> Comment
paragraph = simple . (:[]) . Paragraph

monospace :: Text -> CommentInlineContent
monospace = Monospace . (:[]) . TextContent
