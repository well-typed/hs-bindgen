module HsBindgen.Clang.LowLevel.Doxygen.Enums (
    CXCommentKind(..)
  , CXCommentInlineCommandRenderKind(..)
  , CXCommentParamPassDirection(..)
  ) where

-- | Describes the type of the comment AST node ('CXComment').
--
-- A comment node can be considered block content (e. g., paragraph), inline
-- content (plain text) or neither (the root AST node).
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga3c336d80551401fde394b84aa5651221>
data CXCommentKind =
    -- | Null comment.
    --
    -- No AST node is constructed at the requested location because there is no
    -- text or a syntax error.
    CXComment_Null

    -- | Plain text.
    --
    -- Inline content.
  | CXComment_Text

    -- | A command with word-like arguments that is considered inline content.
    --
    -- For example: @\\c command@.
  | CXComment_InlineCommand

    -- | HTML start tag with attributes (name-value pairs).
    --
    -- Considered inline content.
    --
    -- For example:
    --
    -- > <br> <br /> <a href="http://example.org/">
  | CXComment_HTMLStartTag

    -- | HTML end tag.
    --
    -- Considered inline content.
    --
    -- For example:
    --
    -- > </a>
  | CXComment_HTMLEndTag

     -- | A paragraph, contains inline comment.
     --
     -- The paragraph itself is block content.
  | CXComment_Paragraph

    -- | A command that has zero or more word-like arguments (number of
    -- word-like arguments depends on command name) and a paragraph as an
    -- argument.
   --
   -- Block command is block content.
   --
   -- Paragraph argument is also a child of the block command.
   --
   -- For example: @\has 0 word-like arguments and a paragraph argument@.
   --
   -- AST nodes of special kinds that parser knows about (e. g., @\param@
   -- command) have their own node kinds.
  | CXComment_BlockCommand

   -- | A @\param@ or @\arg@ command that describes the function parameter
   -- (name, passing direction, description).
   --
   -- For example: @\param [in] ParamName description@.
  | CXComment_ParamCommand

   -- | A @\tparam@ command that describes a template parameter (name and
   -- description).
   --
   -- For example: @\tparam T description@.
  | CXComment_TParamCommand

    -- | A verbatim block command (e. g., preformatted code).
    --
    -- Verbatim block has an opening and a closing command and contains multiple
    -- lines of text ('CXComment_VerbatimBlockLine' child nodes).
    --
    -- For example:
    --
    -- > \verbatim
    -- > aaa
    -- > \endverbatim
  | CXComment_VerbatimBlockCommand


    -- | A line of text that is contained within a
    -- 'CXComment_VerbatimBlockCommand' node.
  | CXComment_VerbatimBlockLine

    -- | A verbatim line command.
    --
    -- Verbatim line has an opening command, a single line of text (up to the
    -- newline after the opening command) and has no closing command.
  | CXComment_VerbatimLine

    -- | A full comment attached to a declaration, contains block content.
  | CXComment_FullComment
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | The most appropriate rendering mode for an inline command, chosen on
-- command semantics in Doxygen.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga23efacd9c1e4e286a9f9714e1720fdcf>
data CXCommentInlineCommandRenderKind =
    -- | Command argument should be rendered in a normal font.
    CXCommentInlineCommandRenderKind_Normal

    -- | Command argument should be rendered in a bold font.
  | CXCommentInlineCommandRenderKind_Bold

    -- | Command argument should be rendered in a monospaced font.
  | CXCommentInlineCommandRenderKind_Monospaced

    -- | Command argument should be rendered emphasized (typically italic
    -- font).
  | CXCommentInlineCommandRenderKind_Emphasized

    -- | Command argument should not be rendered (since it only defines an
    -- anchor).
  | CXCommentInlineCommandRenderKind_Anchor
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Describes parameter passing direction for @\\param@ or @\\arg@ command.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gafadf6e52217ea74d1a014198df656ee1>
data CXCommentParamPassDirection =
    -- | The parameter is an input parameter.
    CXCommentParamPassDirection_In

    -- | The parameter is an output parameter.
  | CXCommentParamPassDirection_Out

    -- | The parameter is an input and output parameter.
  | CXCommentParamPassDirection_InOut
  deriving stock (Show, Eq, Ord, Enum, Bounded)