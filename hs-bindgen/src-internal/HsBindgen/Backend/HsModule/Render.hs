{-# LANGUAGE MagicHash #-}

-- This still uses RecordWildCards for similar reasons as
-- HsBindgen.Backend.Hs.Haddock.Translation; see discussion there.
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsBindgen.Backend.HsModule.Render (
    -- * Rendering
    render
  , renderWrappers
    -- * Rendering comments
  , CommentKind (..)
  ) where

import Data.Char qualified
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Word
import GHC.Exts (Int (..), sizeofByteArray#)
import GHC.Exts qualified as IsList (IsList (..))
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Text.SimplePrettyPrint (CtxDoc, Pretty (..), ($$), ($+$), (<+>), (><))
import Text.SimplePrettyPrint qualified as PP

import C.Char qualified as CExpr.Runtime

import C.Expr.Syntax qualified as CExpr.DSL

import Clang.HighLevel.Types

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type (HsPrimType (..))
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.HsModule.Capi (renderCapiWrapper)
import HsBindgen.Backend.HsModule.Names
import HsBindgen.Backend.HsModule.Translation
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.SHs.Translation (translateType)
import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.RootHeader (HashIncludeArg (..))
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint

import DeBruijn (Add (..), EmptyCtx, Env (..), lookupEnv)
import Numeric (showHex)

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

-- | Render generated bindings
render :: HsModule -> String
render = (++ "\n") . PP.renderPretty (PP.mkContext 80)

{-------------------------------------------------------------------------------
  Module pretty-printing
-------------------------------------------------------------------------------}

instance Pretty HsModule where
  pretty hsModule = PP.vsep $
      PP.vcat (map pretty hsModule.pragmas)
    : PP.hsep ["module", PP.string (Hs.moduleNameToString hsModule.name), "where"]
    : PP.vcat (map pretty hsModule.imports)
    : (renderWrappers hsModule.cWrappers)
    : map pretty hsModule.decls

{-------------------------------------------------------------------------------
  GhcPragma pretty-printing
-------------------------------------------------------------------------------}

instance Pretty GhcPragma where
  pretty (GhcPragma ghcPragma) = PP.hsep ["{-#", PP.string ghcPragma, "#-}"]

{-------------------------------------------------------------------------------
  Import pretty-printing
-------------------------------------------------------------------------------}

resolve :: Global -> BackendName
resolve = ResolvedBackendName . resolveGlobal

instance Pretty ImportListItem where
  pretty = \case
    UnqualifiedImportListItem hsImport Nothing -> PP.hsep
      [ "import"
      , PP.string (Hs.moduleNameToString hsImport.name)
      ]
    UnqualifiedImportListItem hsImport (Just ns) -> PP.hsep
      [ "import"
      , PP.string (Hs.moduleNameToString hsImport.name)
      , PP.parens . PP.hcat . List.intersperse ", " $ map pretty ns
      ]
    QualifiedImportListItem hsImport -> case hsImport.alias of
      Just q -> PP.hsep
        [ "import qualified"
        , PP.string (Hs.moduleNameToString hsImport.name)
        , "as"
        , PP.string q
        ]
      Nothing -> PP.hsep
        [ "import qualified"
        , PP.string (Hs.moduleNameToString hsImport.name)
        ]


{-------------------------------------------------------------------------------
  Comment pretty-printing
-------------------------------------------------------------------------------}

-- | Here we generate valid Haddock for 'Hs.Comment'. There are roughly 4 types
-- of Haddocks that we might be able to generate:
--
-- * Module Description Commments: Unfortunately, libclang doesn't allow us to
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
    -- ^ Comments that will beging with \"{-|\" for top level declarations
  | PartOfDeclarationComment HsDoc.Comment
    -- ^ Comments that will beging with \"{-^\" for fields and part of
    -- declarations
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
                >< PP.text (escapeAtSigns n)
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
prettyHashIncludeArgLoc info loc = PP.string $
    List.intercalate ":" [
        escapePaths info.includeArg.path
      , show (singleLocLine loc)
      , show (singleLocColumn loc)
      ]

prettyMainHeaders :: C.HeaderInfo -> CtxDoc
prettyMainHeaders info =
      PP.string
    . List.intercalate "@, @"
    . map (escapePaths . (.path))
    . NonEmpty.toList
    $ info.mainHeaders

escapePaths :: String -> String
escapePaths []       = []
escapePaths ('/':ss) = "\\/" ++ escapePaths ss
escapePaths (s:ss)   = s : escapePaths ss

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
  Declaration pretty-printing
-------------------------------------------------------------------------------}

renderWrappers :: [CWrapper] -> CtxDoc
renderWrappers wrappers
  | null src  = PP.empty
  | otherwise = renderCapiWrapper src
  where
    src :: String
    src = getCWrappersSource wrappers

instance Pretty SDecl where
  pretty = \case
    DInst inst ->
      let constraints =
            [ PP.hsep (pretty (resolve c) : (map (prettyPrec 1) ts))
            | (c, ts) <- inst.super
            ]
          -- @flist@ should either be @PP.hlist@ or @PP.vlist@
          clsContext flist = flist "(" ")" constraints
          clsHead = PP.hsep (pretty (resolve inst.clss) : map (prettyPrec 1) inst.args)
          cls flist =
                "instance"
            <+> (if null inst.super
                  then PP.empty
                  else clsContext flist <+> "=>")
            <+> clsHead
            <+> "where"

          instanceHead = PP.ifFits (cls PP.hlist) (cls PP.hlist) (cls PP.vlist)
          typs = flip map inst.types $ \(g, typArgs, typSyn) -> PP.nest 2 $ PP.fsep
            [ "type" <+> ppUnqualBackendName (resolve g) <+> PP.hsep (map (prettyPrec 1) typArgs)
                <+> PP.char '='
            , PP.nest 2 (pretty typSyn)
            ]
          decs = flip map inst.decs $ \(name, expr) -> PP.nest 2 $ PP.fsep
            [ ppUnqualBackendName (resolve name) <+> PP.char '='
            , PP.nest 2 (pretty expr)
            ]
          prettyTopLevelComment = maybe PP.empty (pretty . TopLevelComment) inst.comment

      in  PP.vsep $ prettyTopLevelComment : instanceHead : typs ++ decs

    DRecord record ->
      let d = PP.hsep ["data", pretty record.typ, PP.char '=', pretty record.con]
          prettyTopLevelComment = maybe PP.empty (pretty . TopLevelComment) record.comment
      in  prettyTopLevelComment
       $$ ( PP.hang d 2 $ PP.vcat [
                PP.vlist "{" "}" [
                       PP.hsep [
                           pretty field.name
                         , "::"
                         , pretty field.typ
                         ]
                    $$ prettyFieldComment
                  | field <- record.fields
                  , let prettyFieldComment = maybe PP.empty (pretty . PartOfDeclarationComment) field.comment
                  ]
              , nestedDeriving record.deriv
              ]
          )

    DEmptyData empty ->
      let prettyComment = maybe PP.empty (pretty . TopLevelComment) empty.comment
      in  prettyComment
        $$ PP.hsep ["data", pretty empty.name]

    DNewtype newtyp ->
      let d = PP.hsep ["newtype", pretty newtyp.name, PP.char '=', pretty newtyp.con]
          prettyComment = maybe PP.empty (pretty . TopLevelComment) newtyp.comment
          prettyFieldComment = maybe PP.empty (pretty . PartOfDeclarationComment) newtyp.field.comment
      in  prettyComment
       $$ ( PP.hang d 2 $ PP.vcat [
                PP.vlist "{" "}" [
                      PP.hsep [
                          pretty newtyp.field.name
                        , "::"
                        , pretty newtyp.field.typ
                        ]
                    $$ prettyFieldComment
                  ]
              , nestedDeriving newtyp.deriv
              ]
          )

    DForeignImport foreignImport ->
      -- Variable names here refer to the syntax of foreign declarations at
      -- <https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1540008.4>
      let -- TODO: We should escape special characters inside these import
          -- strings (at the very least quotes in filenames?)
          callconv, impent :: CtxDoc
          (callconv, impent) =
            case foreignImport.callConv of
              CallConvUserlandCAPI _ -> ("ccall",
                  PP.string $ Text.unpack foreignImport.origName.text
                )
              CallConvGhcCAPI header -> ("capi", PP.hcat [
                  PP.string header
                , PP.string $ Text.unpack foreignImport.origName.text
                ])
              CallConvGhcCCall style -> ("ccall", PP.hcat [
                  case style of
                    ImportAsValue -> ""
                    ImportAsPtr   -> "&"
                , PP.string $ Text.unpack foreignImport.origName.text
                ])

          prettyComment = maybe PP.empty (pretty . TopLevelComment) foreignImport.comment

      in  prettyComment
       $$ PP.hsep [ "foreign import"
               , callconv
               , safety foreignImport.safety
               , "\"" >< impent >< "\""
               , pretty foreignImport.name
               , "::"
               ]
       $$ PP.nest 5 (prettyBindingType foreignImport.parameters foreignImport.result)

    DBinding Binding{..} ->
      let prettyComment = maybe PP.empty (pretty . TopLevelComment) comment
          prettyName    = pretty name
          prettyTyp     = prettyBindingType parameters result
          prettySignature =
            if null parameters; then
              prettyName <+> "::" <+> prettyTyp
            else
              prettyName <+> "::" $$  PP.nest 5 prettyTyp
      in  PP.vcat (map (prettyPragma name) pragmas)
       $$ prettyComment
       $$ prettySignature
       $$ PP.fsep
            [ prettyName <+> PP.char '='
            , PP.nest 2 $ pretty body
            ]

    DDerivingInstance deriv ->
      maybe PP.empty (pretty . TopLevelComment) deriv.comment
        $$ "deriving" <+> strategy deriv.strategy
                      <+> "instance"
                      <+> pretty deriv.typ

    DPatternSynonym patSyn ->
      let prettyComment = maybe PP.empty (pretty . TopLevelComment) patSyn.comment
       in PP.vcat [ prettyComment
               , "pattern" <+> pretty patSyn.name <+> "::" <+> pretty patSyn.typ
               , "pattern" <+> pretty patSyn.name <+> "=" <+> pretty patSyn.rhs
               ]

-- | Nested deriving clauses (as part of a datatype declaration)
nestedDeriving :: [(Hs.Strategy ClosedType, [Global])] -> CtxDoc
nestedDeriving deriv = PP.vcat [
      PP.hsep [
          "deriving"
        , strategy s
        , PP.hcat . concat $ [
              ["("]
            , List.intersperse (", ") $ map (pretty . resolve) clss
            , [")"]
            ]
        ]
    | (s, clss) <- deriv
    ]

strategy :: Hs.Strategy ClosedType -> CtxDoc
strategy Hs.DeriveNewtype  = "newtype"
strategy Hs.DeriveStock    = "stock"
strategy (Hs.DeriveVia ty) = "via" <+> pretty ty

prettyPragma :: Hs.Name Hs.NsVar -> Pragma -> CtxDoc
prettyPragma n = \case
  NOINLINE -> "{-# NOINLINE" <+> pretty n <+> "#-}"

-- TODO <https://github.com/well-typed/hs-bindgen/issues/94>
-- We should generate both safe and unsafe bindings.
safety :: Safety -> CtxDoc
safety Safe = "safe"
safety Unsafe = "unsafe"

{-------------------------------------------------------------------------------
  Type pretty-printing
-------------------------------------------------------------------------------}

instance ctx ~ EmptyCtx => Pretty (SType ctx) where
  prettyPrec = prettyType EmptyEnv

prettyBindingType :: [Parameter] -> Result -> CtxDoc
prettyBindingType params result =
  case params of
    [] -> prettyResultType result.typ
    _  -> prettyParams params
  where
    prettyParam p =
      case p.typ of
        TFun {} -> prettyType EmptyEnv 1 p.typ
        _       -> prettyType EmptyEnv 0 p.typ
      $$ maybe PP.empty (pretty . PartOfDeclarationComment) p.comment


    prettyResultType t = prettyType EmptyEnv 0 t

    prettyParams []     = prettyResultType result.typ
    prettyParams (p:ps) =
         prettyParam p
      $$ PP.nest (-3) ("->" <+> prettyParams ps)

prettyType :: Env ctx CtxDoc -> Int -> SType ctx -> CtxDoc
prettyType env prec = \case
    TGlobal g -> pretty $ resolve g
    TCon n -> pretty n
    TFree var -> pretty var
    TLit n -> PP.show n
    TStrLit s -> PP.string (show s)
    TExt i _cTypeSpec _hsTypeSpec -> pretty i
    TApp c x -> PP.parensWhen (prec > 0) $
      prettyType env 1 c <+> prettyType env 1 x
    TFun a b -> PP.parensWhen (prec > 0) $
      prettyType env 1 a <+> "->" <+> prettyType env 0 b
    TBound x -> lookupEnv x env
    TForall hints add ctxt body ->
      case add of
        AZ -> PP.hsep (map (\ ct -> prettyType env 0 ct <+> "=> ") ctxt) >< prettyType env 0 body
        _  -> withFreshNames env add hints $ \env' params ->
          "forall" <+> PP.hsep params >< "." <+>
          PP.hsep (map (\ ct -> prettyType env' 0 ct <+> "=>") ctxt) <+> prettyType env' 0 body

prettyPrimType :: HsPrimType -> CtxDoc
prettyPrimType = prettyType EmptyEnv 0 . TGlobal . PrimType

{-------------------------------------------------------------------------------
  PatEpxr pretty-printing
-------------------------------------------------------------------------------}

instance Pretty PatExpr where
  prettyPrec = prettyPatExpr

prettyPatExpr :: Int -> PatExpr -> CtxDoc
prettyPatExpr prec = \case
    PELit i -> PP.parensWhen (i < 0) $ PP.show i
    PEApps n ps -> PP.parensWhen (prec > 3) $ pretty n <+> PP.hsep (map (prettyPatExpr 4) ps)

{-------------------------------------------------------------------------------
  Expression pretty-printing
-------------------------------------------------------------------------------}

instance ctx ~ EmptyCtx => Pretty (SExpr ctx) where
  prettyPrec = prettyExpr EmptyEnv

prettyExpr :: Env ctx CtxDoc -> Int -> SExpr ctx -> CtxDoc
prettyExpr env prec = \case
    EGlobal g -> pretty $ resolve g

    EBound x -> lookupEnv x env
    EFree x  -> pretty x
    ECon n   -> pretty n

    EIntegral i Nothing -> PP.parensWhen (prec > 0 && i < 0) (PP.show i)
    EUnboxedIntegral i ->
      PP.parens $ PP.hcat [PP.show i, "#"]
    EIntegral i (Just t) ->
      PP.parens $ PP.hcat [PP.show i, " :: ", prettyPrimType t]
    EChar (CExpr.Runtime.CharValue { charValue = ba, unicodeCodePoint = mbUnicode }) ->
      prettyExpr env 0 (EGlobal CharValue_fromAddr)
        <+> PP.string str
        <+> PP.string (show len)
        <+> case mbUnicode of
            { Nothing -> pretty (resolve Maybe_nothing)
            ; Just c -> PP.parens (pretty (resolve Maybe_just) <+> PP.string (show c))
            }
      where
        (str, len) = addrLiteral ba
    EString s -> PP.show s
    ECString bs ->
      -- Use unboxed Addr# literals to turn a PP.string literal into a
      -- value of type CStringLen.
      let (str, len) = addrLiteral bs
      in PP.parens $ PP.hcat
        [ PP.parens $ prettyExpr env 0 (EGlobal Ptr_constructor) <+> PP.string str >< ", " >< PP.string (show len)
        , " :: "
        , prettyPrimType HsPrimCStringLen
        ]

    EFloat f t -> PP.parens $ PP.hcat [
        if CExpr.DSL.canBeRepresentedAsRational f then
          PP.show f
        else
          prettyExpr env prec $
            EApp (EGlobal CFloat_constructor) $
              EApp (EGlobal GHC_Float_castWord32ToFloat) $
                EIntegral (toInteger $ castFloatToWord32 f) (Just HsPrimCUInt)
      , " :: "
      , prettyPrimType t
      ]
    EDouble f t -> PP.parens $ PP.hcat [
        if CExpr.DSL.canBeRepresentedAsRational f then
          PP.show f
        else
          prettyExpr env  prec $
            EApp (EGlobal CDouble_constructor) $
              EApp (EGlobal GHC_Float_castWord64ToDouble) $
                EIntegral (toInteger $ castDoubleToWord64 f) (Just HsPrimCULong)
      , " :: "
      , prettyPrimType t
      ]

    EApp f x -> PP.parensWhen (prec > 3) $ prettyExpr env 3 f <+> prettyExpr env 4 x

    e@(EInfix op x y) -> case (prec, getInfixSpecialCase env e) of
      -- Handle special cases only at precedence 0.
      (0, Just ds) -> PP.vcat ds
      -- Sub-expressions are aggresively parenthesized so that we do not have to
      -- worry about operator fixity/precedence.
      _otherwise ->
        PP.parens $ PP.hsep
          [ prettyExpr env 1 x
          , ppInfixBackendName (resolve op)
          , prettyExpr env 1 y
          ]

    ELam (NameHint hint) body -> PP.withFreshName hint $ \x -> PP.parensWhen (prec > 1) $ PP.fsep
      [ PP.char '\\' >< x <+> "->"
      , PP.nest 2 $ prettyExpr (env :> x) 0 body
      ]

    EUnusedLam body -> PP.parensWhen (prec > 1) $ PP.fsep
      [ PP.char '\\' >< "_" <+> "->"
      , PP.nest 2 $ prettyExpr env 0 body
      ]

    ECase x alts -> PP.vparensWhen (prec > 1) $
      if null alts
        then PP.hsep ["case", prettyExpr env 0 x, "of", "{}"]
        else PP.hang (PP.hsep ["case", prettyExpr env 0 x, "of"]) 2 $ PP.vcat
            ([ withFreshNames env add hints $ \env' params ->

                let l = PP.hsep $ pretty cnst : params ++ ["->"]
                in  PP.ifFits l (PP.fsep [l, PP.nest 2 (prettyExpr env' 0 body)]) $
                    case unsnoc params of
                      Nothing -> PP.fsep [l, PP.nest 2 (prettyExpr env' 0 body)]
                      Just (lParams, rParam) -> PP.vcat $
                          pretty cnst
                        : [ PP.nest 2 param
                          | param <- lParams
                          ]
                        ++ [PP.nest 2 (rParam <+> "->")]
                        ++ [PP.nest 4 (prettyExpr env' 0 body)]

            | SAlt cnst add hints body <- alts
            ]
            ++
            [ withFreshNames env (AS AZ) hints $ \env' params ->
                let l = PP.hsep $ params ++ ["->"]
                in  PP.ifFits l (PP.fsep [l, PP.nest 2 (prettyExpr env' 0 body)]) $
                    case unsnoc params of
                      Nothing -> PP.fsep [l, PP.nest 2 (prettyExpr env' 0 body)]
                      Just (lParams, rParam) -> PP.vcat $
                          [ PP.nest 2 param
                          | param <- lParams
                          ]
                        ++ [PP.nest 2 (rParam <+> "->")]
                        ++ [PP.nest 4 (prettyExpr env' 0 body)]

            | SAltNoConstr hints body <- alts
            ]
            ++
            [ withFreshNames env add hints $ \env' params ->
                let l  = PP.hlist "(# " " #)" params <+> "->"
                in  PP.ifFits l (PP.fsep [l, PP.nest 2 (prettyExpr env' 0 body)]) $
                    case unsnoc params of
                      Nothing -> PP.fsep [l, PP.nest 2 (prettyExpr env' 0 body)]
                      Just (lParams, rParam) -> PP.vcat $
                          [ PP.nest 2 param
                          | param <- lParams
                          ]
                        ++ [PP.nest 2 (rParam <+> "->")]
                        ++ [PP.nest 4 (prettyExpr env' 0 body)]

            | SAltUnboxedTuple add hints body <- alts
            ]
            )

    ETup xs ->
      let ds = prettyExpr env 0 <$> xs
          l  = PP.hlist "(" ")" ds
      in  PP.ifFits l l $ PP.vlist "(" ")" ds
    EUnboxedTup xs ->
      let ds = prettyExpr env 0 <$> xs
          l  = PP.hlist "(# " " #)" ds
      in  PP.ifFits l l $ PP.vlist "(# " " #)" ds
    EList xs ->
      let ds = prettyExpr env 0 <$> xs
          l  = PP.hlist "[" "]" ds
      in  PP.ifFits l l $ PP.vlist "[" "]" ds

    -- NOTE: the precedence is copied from the @EApp@ case above
    ETypeApp f t -> PP.parensWhen (prec > 3) $ prettyExpr env 3 f <+> "@" >< prettyPrec 4 t

-- | Returns the unboxed @Addr#@ literal for the given 'ByteArray', together
-- with its length.
addrLiteral :: ByteArray -> (String, Int)
addrLiteral ba@(ByteArray ba#) =
  let
    go :: Bool -> [Word8] -> String
    go _ [] = ""
    go prevHex (b:bs)
      | Just s <- escapeHsChar_maybe c
      = s ++ go False bs
      | b <= 0x7F
      , Data.Char.isPrint c
      = ( if prevHex then ( "\\&" ++ ) else id ) $
          c : go False bs
      | otherwise
      = "\\x" ++ map Data.Char.toUpper (showHex b "") ++ go True bs
      where
        c = Data.Char.chr $ fromIntegral b
    lit :: String
    lit = "\"" <> go False (IsList.toList ba) <> "\"#"
  in (lit, I# (sizeofByteArray# ba#))

escapeHsChar_maybe :: Char -> Maybe String
escapeHsChar_maybe c =
  case lookup c hsEscapes of
    Nothing -> Nothing
    Just e -> Just ['\\', e]

hsEscapes :: [(Char, Char)]
hsEscapes =
  [ ( '\''  , '\'' ) -- single quote
  , ( '\"'  , '\"' ) -- double quote
  , ( '\\'  , '\\' ) -- backslash
  , ( '\f'  , 'f'  ) -- form feed - new page
  , ( '\t'  , 't'  ) -- horizontal tab
  , ( '\v'  , 'v'  ) -- vertical tab
  , ( '\a'  , 'a'  ) -- audible bell
  , ( '\b'  , 'b'  ) -- backspace
  , ( '\n'  , 'n'  ) -- line feed - new line
  , ( '\r'  , 'r'  ) -- carriage return
  , ( '\NUL', '0'  ) -- null character
  ]

withFreshNames ::
     Env ctx CtxDoc
  -> Add n ctx ctx'
  -> Vec n NameHint
  -> (Env ctx' CtxDoc -> [CtxDoc] -> CtxDoc)
  -> CtxDoc
withFreshNames env AZ     _                         kont = kont env []
withFreshNames env (AS a) (NameHint hint ::: hints) kont = PP.withFreshName hint $ \name ->
    withFreshNames env a hints $ \env' names -> kont (env' :> name) (name : names)

getInfixSpecialCase :: forall ctx. Env ctx CtxDoc -> SExpr ctx -> Maybe [CtxDoc]
getInfixSpecialCase env = \case
    EInfix op x y ->
      let opDoc = ppInfixBackendName $ resolve op
      in  case op of
            Applicative_seq -> auxl op opDoc [opDoc <+> prettyExpr env 1 y] x
            Monad_seq       -> auxr op opDoc [sp opDoc <+> prettyExpr env 1 x] y
            _otherwise      -> Nothing
    _otherwise -> Nothing
  where
    -- | Handle left-associative special cases
    auxl ::
         Global   -- ^ operator
      -> CtxDoc   -- ^ operator document
      -> [CtxDoc] -- ^ accumulated lines
      -> SExpr ctx -- ^ left expression
      -> Maybe [CtxDoc]
    auxl op opDoc acc = \case
      EInfix op' x y
        | op' == op -> auxl op opDoc (opDoc <+> prettyExpr env 1 y : acc) x
        | otherwise -> Nothing
      e -> Just $ sp opDoc <+> prettyExpr env 1 e : acc

    -- | Handle right-associative special cases
    auxr ::
         Global   -- ^ operator
      -> CtxDoc   -- ^ operator document
      -> [CtxDoc] -- ^ accumulated lines in reverse order
      -> SExpr ctx -- ^ right expression
      -> Maybe [CtxDoc]
    auxr op opDoc acc = \case
      EInfix op' x y
        | op' == op -> auxr op opDoc (opDoc <+> prettyExpr env 1 x : acc) y
        | otherwise -> Nothing
      e -> Just . reverse $ opDoc <+> prettyExpr env 1 e : acc

    -- | Create document of spaces that has same width as passed document
    sp :: CtxDoc -> CtxDoc
    sp =
      -- TODO compute column width, do not just count chars with length
      PP.string . flip List.replicate ' ' . length . PP.renderCtxDoc PP.defaultContext

{-------------------------------------------------------------------------------
  Hs.Name pretty-printing
-------------------------------------------------------------------------------}

instance Pretty (Hs.Name ns) where
  pretty = PP.text . Hs.getName

{-------------------------------------------------------------------------------
  ResolvedName pretty-printing
-------------------------------------------------------------------------------}

-- | Pretty-print a 'ResolvedName' in prefix notation
--
-- Operators are parenthesized.
instance Pretty ResolvedName where
  pretty resolved =
      PP.parensWhen (resolved.typ == OperatorName) $ ppResolvedName resolved

-- | Pretty-print a 'ResolvedName'
--
-- This auxialary function pretty-prints without parenthesizing operators or
-- surrounding identifiers with backticks.
ppResolvedName :: ResolvedName -> CtxDoc
ppResolvedName resolved =
    case resolved.hsImport of
      Just (QualifiedHsImport hsImport) ->
        let q = fromMaybe
                  (Hs.moduleNameToString hsImport.name)
                  hsImport.alias
        in  PP.string $ q ++ '.' : resolved.string
      _otherwise ->
        PP.string resolved.string

{-------------------------------------------------------------------------------
  BackendName pretty-printing
-------------------------------------------------------------------------------}

-- | Pretty-print a 'BackendName' in prefix notation
--
-- Operators are parenthesized.
instance Pretty BackendName where
  pretty = \case
    LocalBackendName nameType s ->
      PP.parensWhen (nameType == OperatorName) $ PP.string s
    ResolvedBackendName n -> pretty n

-- | Pretty-print a 'BackendName' unqualified
--
-- This is needed in instance declarations.
ppUnqualBackendName :: BackendName -> CtxDoc
ppUnqualBackendName = \case
    LocalBackendName nameType s ->
      PP.parensWhen (nameType == OperatorName) $ PP.string s
    ResolvedBackendName resolved ->
      PP.parensWhen (resolved.typ == OperatorName) $ PP.string resolved.string

-- | Pretty-print a 'BackendName' in infix notation
--
-- Identifiers are surrounded by backticks.
ppInfixBackendName :: BackendName -> CtxDoc
ppInfixBackendName = \case
    LocalBackendName nameType s ->
      bticksWhen (nameType == IdentifierName) $ PP.string s
    ResolvedBackendName resolved ->
      bticksWhen (resolved.typ == IdentifierName) $ ppResolvedName resolved
  where
    bticksWhen :: Bool -> CtxDoc -> CtxDoc
    bticksWhen False d = d
    bticksWhen True  d = PP.hcat [PP.char '`', d, PP.char '`']

{-------------------------------------------------------------------------------
  ExtIdentifier pretty-printing
-------------------------------------------------------------------------------}

instance Pretty Hs.ModuleName where
  pretty = PP.string . Hs.moduleNameToString

instance Pretty Hs.Identifier where
  pretty = PP.string . Text.unpack . (.text)

instance Pretty Hs.ExtRef where
  pretty extRef = PP.hcat [
        pretty extRef.moduleName
      , PP.char '.'
      , pretty extRef.ident
      ]

{-------------------------------------------------------------------------------
  Auxiliary Functions
-------------------------------------------------------------------------------}

-- | In "Data.List" from @base-4.19.0.0@
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
{-# INLINABLE unsnoc #-}

escapeAtSigns :: Text -> Text
escapeAtSigns = Text.pack . concatMap aux . Text.unpack
  where
    aux :: Char -> [Char]
    aux '@' = "\\@"
    aux c   = [c]

