{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsBindgen.Backend.HsModule.Pretty.Comment (
    CommentKind(..)
  ) where

import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import DeBruijn (Env (..))
import Text.SimplePrettyPrint (CtxDoc, Pretty (..), ($$), ($+$), (<+>), (><))
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types

import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.HsModule.Pretty.Type
import HsBindgen.Backend.SHs.Translation
import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Comment pretty-printing
-------------------------------------------------------------------------------}

-- | Here we generate valid Haddock for 'Hs.Comment'. There are roughly 4 types
-- of Haddocks that we might be able to generate:
--
-- * Module Description Comments: Unfortunately, libclang doesn't allow us to
-- parse module level comments because they are not associated with any AST
-- node. Assuming that the comment is not immediately followed by a
-- declaration, in that case the module level comment will get confused with a
-- top level declaration comment.
--
-- * Top Level Comments: These comments are the top level comments for any
-- declaration.
--
-- * Parts of a Declaration Comments: In addition to documenting the whole
-- declaration, in some cases we can also document individual parts of the declaration.
--
-- * Template Haskell Comments: These comments can be either top level or
-- parts of a declaration, but won't carry any specific documentation PP.string
-- like \"--\".

-- As mentioned above Libclang can only parse comments that immediately before
-- a supported declaration. Any comments before a not supported declaration,
-- e.g. macros, will be lost.

-- With this being said we can only do a best effort to generate Top Level and
-- Parts of a Declaration documentation. The following data type distinguishes
-- these two.
--
data CommentKind
  = TopLevelComment HsDoc.Comment
    -- ^ Comments that begin with \"{-|\" for top level declarations
  | PartOfDeclarationComment HsDoc.Comment
    -- ^ Comments that begin with \"{-^\" for fields and part of declarations
  | THComment HsDoc.Comment
    -- ^ Comments that will not begin with any specific documentation PP.string
    -- since they will be taken care of by Template Haskell

instance Pretty CommentKind where
  pretty commentKind =
    let (commentStart, commentEnd, comment) =
          case commentKind of
            TopLevelComment c          -> ("{-|", "-}", c)
            PartOfDeclarationComment c -> ("{- ^", "-}", c)
            THComment c                -> ("", "", c)
        indentation = length commentStart - 1
        -- Separate user-facing metadata (for documentation) from internal metadata.
        -- Only user-facing metadata should trigger Haddock comment syntax.
        userFacingMetadata = catMaybes [
            (\n -> "__C declaration:__ @"
                >< PP.text (escapeMidLine n)
                >< "@") <$> comment.origin
          , (\p -> "__defined at:__ @"
                >< uncurry prettyHashIncludeArgLoc p
                >< "@"
            ) <$> (liftA2 (,) comment.headerInfo comment.location)
          , (\hinfo -> "__exported by:__ @"
                    >< prettyMainHeaders hinfo
                    >< "@") <$> comment.headerInfo
          ]
        internalMetadata = catMaybes [
            (\u -> "__unique:__ @"
               >< PP.string u.source
               >< "@"
            ) <$> comment.unique
          ]
        allMetadata = userFacingMetadata ++ internalMetadata
        firstContent =
          case comment.title of
            Nothing -> PP.empty
            Just ct -> PP.hsep (map pretty ct)
        singleLineStart =
          case commentKind of
            TopLevelComment _          -> "-- |"
            PartOfDeclarationComment _ -> "-- ^"
            THComment _                -> ""
        -- If the comment only has the the origin C Name then use that has the
        -- title.
     in case comment.children of
          [] | Nothing <- comment.title
             , [singleMetadata] <- userFacingMetadata ->
                -- Single user-facing metadata: use Haddock single-line style
                PP.string singleLineStart <+> singleMetadata
             | Nothing <- comment.title
             , null userFacingMetadata
             , [singleMetadata] <- internalMetadata ->
                -- Only internal metadata: use regular comment
                "--" <+> singleMetadata
             | Nothing <- comment.title
             , not (null allMetadata) ->
                PP.string commentStart
            <+> PP.vsep allMetadata
             $$ PP.string commentEnd
             | Just _ <- comment.title
             , null allMetadata ->
                PP.string commentStart
            <+> firstContent
             $$ PP.string commentEnd
             | Just _  <- comment.title
             , not (null allMetadata) ->
                PP.string commentStart
            <+> firstContent
            $+$ PP.vsep allMetadata
             $$ PP.string commentEnd
             | otherwise -> PP.empty

          _ -> PP.vsep (PP.string commentStart <+> firstContent
                     : map (PP.nest indentation . pretty) comment.children)
            $+$ PP.vcat [ PP.vsep allMetadata
                     , PP.string commentEnd
                     ]

prettyHashIncludeArgLoc :: C.HeaderInfo -> SingleLoc -> CtxDoc
prettyHashIncludeArgLoc info loc =
    -- Text like @:1:2@ is mangled by the GHC literate preprocessor, so we
    -- cannot format source locations like that.
    --
    -- * @foo.h 1:2@ is fine
    -- * @foo.h:1:2@ causes mangling issues
    PP.string . escapeMidLineString . unwords $ catMaybes [
        Just info.includeArg.path
      , formatMacroArg <$> info.includeMacroArg
      , Just sourceLoc
      ]
  where
    sourceLoc :: String
    sourceLoc = show (singleLocLine loc) ++ ':' : show (singleLocColumn loc)

    formatMacroArg :: Text -> String
    formatMacroArg = ('(' :) . (++ ")") . Text.unpack

prettyMainHeaders :: C.HeaderInfo -> CtxDoc
prettyMainHeaders info =
      PP.string
    . List.intercalate "@, @"
    . map (escapeMidLineString . (.path))
    . NonEmpty.toList
    $ info.mainHeaders

instance Pretty HsDoc.CommentBlockContent where
  pretty = \case
    HsDoc.Paragraph{..}      -> PP.hsep
                              . map pretty
                              $ paragraphContent
    HsDoc.CodeBlock{..}      -> PP.vcat
                              $ ["@"]
                             ++ map PP.text codeBlockLines
                             ++ ["@"]
    HsDoc.Verbatim{..}       -> ">" <+> PP.text verbatimContent
    HsDoc.Example{..}        -> ">>>" <+> PP.text exampleContent
    HsDoc.Property{..}       -> "prop>" <+> PP.text propertyContent
    HsDoc.ListItem{..}       ->
      let listMarker =
            case listItemType of
              HsDoc.BulletList -> "*"
              HsDoc.NumberedList n -> PP.show n >< "."
       in listMarker <+> PP.vcat (map pretty listItemContent)
    HsDoc.DefinitionList{..} -> "["
                             >< pretty definitionListTerm
                             >< "]:"
                            <+> PP.vcat (map pretty definitionListContent)
    HsDoc.Header{..}         -> PP.string (replicate (fromEnum headerLevel) '=')
                            <+> (PP.hsep $ map pretty headerContent)


instance Pretty HsDoc.CommentInlineContent where
  pretty = \case
    HsDoc.TextContent{..}   -> PP.text textContent
    HsDoc.Monospace{..}     -> "@" >< PP.hsep (map pretty monospaceContent) >< "@"
    HsDoc.Emph{..}          -> "/" >< PP.hsep (map pretty emphContent) >< "/"
    HsDoc.Bold{..}          -> "__" >< PP.hsep (map pretty boldContent) >< "__"
    HsDoc.Module{..}        -> "\"" >< PP.text moduleContent >< "\""
    HsDoc.Identifier{..}    -> "'" >< PP.text identifierContent >< "'"
    HsDoc.Type{..}          -> "t'" >< PP.text typeContent
    HsDoc.Link{..}          -> "[" >< PP.hsep (map pretty linkLabel) >< "]"
                            >< "(" >< PP.text linkURL >< ")"
    HsDoc.URL{..}           -> "<" >< PP.text urlContent >< ">"
    HsDoc.Anchor{..}        -> "#" >< PP.text anchorContent >< "#"
    HsDoc.Math{..}          -> "\\[" >< PP.vcat (map PP.text mathContent) >< "\\]"
    HsDoc.Metadata{..}      -> pretty metadataContent
    HsDoc.TypeSignature{..} -> "@" >< prettyType EmptyEnv 0 (translateType typeSignature) >< "@"

instance Pretty HsDoc.CommentMeta where
  pretty HsDoc.Since{..} = "@since:" <+> PP.text sinceContent

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- | Escape Haddock special characters in mid-line content
--
-- <https://haskell-haddock.readthedocs.io/latest/markup.html#special-characters>
--
-- This is /not/ sufficient to escape start-of-line content.
escapeMidLine :: Text -> Text
escapeMidLine = Text.pack . escapeMidLineString . Text.unpack

escapeMidLineString :: String -> String
escapeMidLineString = concatMap aux
  where
    aux :: Char -> [Char]
    aux = \case
      '\\' -> "\\\\"
      '/'  -> "\\/"
      '\'' -> "\\'"
      '`'  -> "\\`"
      '"'  -> "\\\""
      '@'  -> "\\@"
      '<'  -> "\\<"
      '$'  -> "\\$"
      '#'  -> "\\#"
      c    -> [c]
