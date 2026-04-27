-- | Enrich parsed declarations with doxygen comments
--
-- Post-processing step after @EnrichComments@: looks up each declaration's
-- comment in the 'Doxygen' state and fills in @DeclInfo.comment@ and
-- per-field comments.
--
-- === Doxygen nesting
--
-- Doxygen and libclang see nested structs differently. Given:
--
-- @
-- struct outer {
--     struct inner {
--         int x;
--         struct {
--             int a;
--             struct inner_inner_inner {
--                 int c;
--             };
--         } inner_inner;
--     } field;
--     int z;
-- };
-- @
--
-- Doxygen produces one XML file per named compound, using @\"::\"@-qualified
-- names. Named nested structs get their own file; anonymous structs are
-- flattened into the nearest named enclosing struct:
--
-- @
-- \<!-- outer.xml -->
-- \<compounddef kind=\"struct\">
--   \<compoundname>outer\</compoundname>
--   \<sectiondef kind=\"public-attrib\">
--     \<memberdef kind=\"variable\">\<name>field\</name>\</memberdef>
--     \<memberdef kind=\"variable\">\<name>z\</name>\</memberdef>
--   \</sectiondef>
-- \</compounddef>
--
-- \<!-- outer::inner.xml — note: field \"a\" is flattened from the anon struct -->
-- \<compounddef kind=\"struct\">
--   \<compoundname>outer::inner\</compoundname>
--   \<sectiondef kind=\"public-attrib\">
--     \<memberdef kind=\"variable\">\<name>x\</name>\</memberdef>
--     \<memberdef kind=\"variable\">\<name>a\</name>\</memberdef>
--     \<memberdef kind=\"variable\">\<name>inner_inner\</name>\</memberdef>
--   \</sectiondef>
-- \</compounddef>
--
-- \<!-- outer::inner::inner_inner_inner.xml -->
-- \<compounddef kind=\"struct\">
--   \<compoundname>outer::inner::inner_inner_inner\</compoundname>
--   \<sectiondef kind=\"public-attrib\">
--     \<memberdef kind=\"variable\">\<name>c\</name>\</memberdef>
--   \</sectiondef>
-- \</compounddef>
-- @
--
-- The @doxygen-parser@ library turns this into a
-- @'Map' 'DoxygenKey' ('Comment' 'Text')@ with keys like:
--
-- @
-- KeyStruct \"outer\"                                 -- compound doc
-- KeyField  \"outer\" \"field\"                       -- field doc
-- KeyStruct \"outer::inner\"                          -- compound doc
-- KeyField  \"outer::inner\" \"x\"                    -- field doc
-- KeyField  \"outer::inner\" \"a\"                    -- flattened from anon
-- KeyField  \"outer::inner\" \"inner_inner\"          -- anon struct's doc
-- KeyStruct \"outer::inner::inner_inner_inner\"       -- compound doc
-- KeyField  \"outer::inner::inner_inner_inner\" \"c\" -- field doc
-- @
--
-- === Lookup algorithm
--
-- Each 'DeclInfo' carries @enclosing@: the list of enclosing declarations
-- (innermost first), set during parsing.
--
-- __Named declarations__ ('lookupCommentForId'): 'resolveQualifiedName'
-- reverses the @enclosing@ list, collects named ancestors (skipping
-- anonymous ones), and joins with @\"::\"@.  Then we look up
-- @'KeyDecl' qualName \<|\> 'KeyStruct' qualName@.
--
--  * @inner_inner_inner@ → enclosing list @[\<anon>, inner, outer]@, skip
--    anon → @\"outer::inner::inner_inner_inner\"@
--
-- Anonymous declarations have no doxygen comment of their own: doxygen
-- associates the comment with the enclosing field, which we pick up via
-- 'enrichStructField'\/'enrichUnionField' below.
--
-- __Fields__ ('enrichStructField', etc.): look up directly with
-- @'KeyField' enclosingQualName fieldName@ (or 'KeyEnumValue').
--
module HsBindgen.Frontend.Pass.EnrichComments (enrichComments) where

import Control.Applicative ((<|>))
import Data.Text qualified as Text

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass (AssignAnonIds)
import HsBindgen.Frontend.Pass.EnrichComments.IsPass (EnrichComments)
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Imports

import Doxygen.Parser (Doxygen, DoxygenKey (..), lookupComment)
import Doxygen.Parser.Types qualified as Doxy

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Enrich parsed declarations with doxygen comments
enrichComments :: Doxygen -> [ParseResult AssignAnonIds] -> [ParseResult EnrichComments]
enrichComments doxy results =
    map enrichOne coerced
  where
    -- Coerce input to 'EnrichComments' before enrichment. The coercion sets
    -- every @comment@ field to 'Nothing' (since 'CommentDecl AssignAnonIds'
    -- is @()@ and 'CommentDecl EnrichComments' is @Maybe (Comment EnrichComments)@)
    -- via 'CoercePassCommentDecl'. We then fill in comments by looking up the
    -- doxygen state.
    coerced :: [ParseResult EnrichComments]
    coerced = map coercePass results

    enrichOne :: ParseResult EnrichComments -> ParseResult EnrichComments
    enrichOne pr = case pr.classification of
      ParseResultSuccess success ->
        let decl' = enrichDecl doxy success.decl
        in  pr { classification =
                    ParseResultSuccess success { decl = decl' }
               }
      _ -> pr

{-------------------------------------------------------------------------------
  Declaration-level enrichment
-------------------------------------------------------------------------------}

enrichDecl ::
     Doxygen
  -> C.Decl EnrichComments
  -> C.Decl EnrichComments
enrichDecl doxy decl =
    decl & #info %~ enrichDeclInfo doxy
         & #kind %~ enrichDeclKind doxy effectiveEnclosing
  where
    -- Doxygen-qualified name for field lookups (e.g., @\"outer::inner\"@)
    effectiveEnclosing :: Text
    effectiveEnclosing = resolveQualifiedName decl.info

enrichDeclInfo ::
     Doxygen
  -> C.DeclInfo EnrichComments
  -> C.DeclInfo EnrichComments
enrichDeclInfo doxy info
  | Just doxyComment <- lookupCommentForId doxy info
  = info & #comment .~ Just (wrapDoxygenRefs doxyComment)
  | otherwise
  = info

lookupCommentForId ::
     Doxygen
  -> C.DeclInfo EnrichComments
  -> Maybe (Doxy.Comment Text)
lookupCommentForId doxy info
  | not info.id.isAnon =
      let qualName = resolveQualifiedName info
      in  lookupComment (KeyDecl qualName) doxy
            <|> lookupComment (KeyStruct qualName) doxy
  | otherwise = Nothing

{-------------------------------------------------------------------------------
  Field-level enrichment
-------------------------------------------------------------------------------}

enrichDeclKind ::
     Doxygen
  -> Text  -- ^ Declaration C name (enclosing for field lookups)
  -> C.DeclKind EnrichComments
  -> C.DeclKind EnrichComments
enrichDeclKind doxy name = \case
    C.DeclStruct struct -> C.DeclStruct $ enrichStruct doxy name struct
    C.DeclUnion  union  -> C.DeclUnion  $ enrichUnion  doxy name union
    C.DeclEnum   enum   -> C.DeclEnum   $ enrichEnum   doxy name enum
    C.DeclAnonEnumConstant aec ->
      C.DeclAnonEnumConstant $
        aec & #constant %~ enrichEnumConstant doxy name
    other -> other

enrichStruct :: Doxygen -> Text -> C.Struct EnrichComments -> C.Struct EnrichComments
enrichStruct doxy name struct = struct
    & #fields %~ map (enrichStructField doxy name)
    & #flam   %~ fmap (enrichStructField doxy name)

enrichUnion :: Doxygen -> Text -> C.Union EnrichComments -> C.Union EnrichComments
enrichUnion doxy name union = union
    & #fields %~ map (enrichUnionField doxy name)

enrichEnum :: Doxygen -> Text -> C.Enum EnrichComments -> C.Enum EnrichComments
enrichEnum doxy name enum = enum
    & #constants %~ map (enrichEnumConstant doxy name)

enrichStructField ::
     Doxygen -> Text -> C.StructField EnrichComments -> C.StructField EnrichComments
enrichStructField doxy name sf =
    maybe sf (\c -> sf & #info % #comment .~ Just c) $ do
      lookupFieldComment doxy (KeyField name sf.info.name.text)

enrichUnionField ::
     Doxygen -> Text -> C.UnionField EnrichComments -> C.UnionField EnrichComments
enrichUnionField doxy name uf =
    maybe uf (\c -> uf & #info % #comment .~ Just c) $ do
      lookupFieldComment doxy (KeyField name uf.info.name.text)

enrichEnumConstant ::
     Doxygen -> Text -> C.EnumConstant EnrichComments -> C.EnumConstant EnrichComments
enrichEnumConstant doxy name ec =
    maybe ec (\c -> ec & #info % #comment .~ Just c) $ do
      lookupFieldComment doxy (KeyEnumValue name ec.info.name.text)

{-------------------------------------------------------------------------------
  Enclosing declaration resolution
-------------------------------------------------------------------------------}

-- | Build the @\"::\"@-qualified name that doxygen uses for this declaration.
--
-- Named ancestors are joined with @\"::\"@ (e.g., @outer::inner@).
-- Anonymous ancestors are skipped. Anonymous decls resolve to the nearest
-- named ancestor.
resolveQualifiedName :: C.DeclInfo EnrichComments -> Text
resolveQualifiedName info =
    Text.intercalate "::" (map (.name.text) path)
  where
    path :: [DeclId]
    path =
      filter (not . (.isAnon)) $
      reverse (map getEnclosingRef info.enclosing) ++ [info.id]

    getEnclosingRef :: C.EnclosingRef EnrichComments -> DeclId
    getEnclosingRef = \case
        C.EnclosingRef         x -> x
        C.UnusableEnclosingRef x -> x

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Look up a doxygen comment by key and wrap its cross-references
lookupFieldComment :: Doxygen -> DoxygenKey -> Maybe (C.Comment EnrichComments)
lookupFieldComment doxy key = wrapDoxygenRefs <$> lookupComment key doxy

-- | Wrap doxygen cross-reference 'Text' values into 'C.CommentRef'
wrapDoxygenRefs :: Doxy.Comment Text -> C.Comment EnrichComments
wrapDoxygenRefs comment = C.Comment (fmap wrapRef comment)
  where
    wrapRef :: Text -> C.CommentRef EnrichComments
    wrapRef ref = C.CommentRef ref Nothing
