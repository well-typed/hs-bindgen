{-# LANGUAGE MagicHash #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsBindgen.Backend.Artefact.HsModule.Render (
    -- * Rendering
    render
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
import Text.SimplePrettyPrint

import C.Char (CharValue (..))

import Clang.HighLevel.Types

import HsBindgen.Backend.Artefact.HsModule.Names
import HsBindgen.Backend.Artefact.HsModule.Translation
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type (HsPrimType (..), ResultType (..))
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as Hs
import HsBindgen.Backend.SHs.AST
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.RootHeader (HashIncludeArg (..))
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell
import HsBindgen.NameHint

import DeBruijn (Add (..), EmptyCtx, Env (..), lookupEnv)
import Numeric (showHex)

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

-- | Render generated bindings
render :: HsModule -> String
render = (++ "\n") . renderPretty (mkContext 80)

{-------------------------------------------------------------------------------
  Module pretty-printing
-------------------------------------------------------------------------------}

instance Pretty HsModule where
  pretty HsModule{..} = vsep $
      vcat (map pretty hsModulePragmas)
    : hsep ["module", string hsModuleName, "where"]
    : vcat (map pretty hsModuleImports)
    : map pretty hsModuleDecls

{-------------------------------------------------------------------------------
  GhcPragma pretty-printing
-------------------------------------------------------------------------------}

instance Pretty GhcPragma where
  pretty (GhcPragma ghcPragma) = hsep ["{-#", string ghcPragma, "#-}"]

{-------------------------------------------------------------------------------
  Import pretty-printing
-------------------------------------------------------------------------------}

resolve :: Global -> BackendName
resolve = ResolvedBackendName . resolveGlobal

instance Pretty ImportListItem where
  pretty = \case
    UnqualifiedImportListItem HsImportModule{..} ns -> hsep
      [ "import"
      , string hsImportModuleName
      , parens . hcat . List.intersperse ", " $ map pretty ns
      ]
    QualifiedImportListItem HsImportModule{..} -> case hsImportModuleAlias of
      Just q ->
        hsep ["import qualified", string hsImportModuleName, "as", string q]
      Nothing -> hsep ["import qualified", string hsImportModuleName]


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
-- parts of a declaration, but won't carry any specific documentation string
-- like \"--\".

-- As mentioned above Libclang can only parse comments that immediately before
-- a supported declaration. Any comments before a not supported declaration,
-- e.g. macros, will be lost.

-- With this being said we can only do a best effort to generate Top Level and
-- Parts of a Declaration documentation. The following data type distinguishes
-- these two.
--
data CommentKind
  = TopLevelComment Hs.Comment
    -- ^ Comments that will beging with \"{-|\" for top level declarations
  | PartOfDeclarationComment Hs.Comment
    -- ^ Comments that will beging with \"{-^\" for fields and part of
    -- declarations
  | THComment Hs.Comment
    -- ^ Comments that will not begin with any specific documentation string
    -- since they will be taken care of by Template Haskell

instance Pretty CommentKind where
  pretty commentKind =
    let (commentStart, commentEnd, Hs.Comment {..}) =
          case commentKind of
            TopLevelComment c          -> ("{-|", "-}", c)
            PartOfDeclarationComment c -> ("{- ^", "-}", c)
            THComment c                -> ("", "", c)
        indentation = length commentStart - 1
        fromCCtxDoc =
          case commentOrigin of
            Nothing    -> empty
            Just cName ->
              let parts = [ Just $ "__C declaration:__ @"
                                >< textToCtxDoc cName
                                >< "@"
                          , (\p -> "__defined at:__ @"
                                >< uncurry prettyHashIncludeArgLoc p
                                >< "@") <$> (liftA2 (,) commentHeaderInfo commentLocation)
                          , (\hinfo -> "__exported by:__ @"
                                    >< prettyMainHeaders hinfo
                                    >< "@") <$> commentHeaderInfo
                          ]
              in vsep (catMaybes parts)
        firstContent =
          case commentTitle of
            Nothing -> empty
            Just ct -> hsep (map pretty ct)
        -- If the comment only has the the origin C Name then use that has the
        -- title.
     in case commentChildren of
          [] | Nothing <- commentTitle ->
                string commentStart
            <+> fromCCtxDoc
             $$ string commentEnd
             | otherwise -> empty

          _  -> vsep (string commentStart <+> firstContent
                     : map (nest indentation . pretty) commentChildren)
            $+$ vcat [ fromCCtxDoc
                     , string commentEnd
                     ]

prettyHashIncludeArgLoc :: C.HeaderInfo -> SingleLoc -> CtxDoc
prettyHashIncludeArgLoc hinfo loc = string $
    List.intercalate ":"
      [ getHashIncludeArg (C.headerInclude hinfo)
      , show (singleLocLine loc)
      , show (singleLocColumn loc)
      ]

prettyMainHeaders :: C.HeaderInfo -> CtxDoc
prettyMainHeaders
    = string
    . List.intercalate "@, @"
    . map getHashIncludeArg
    . NonEmpty.toList
    . C.headerMainHeaders

instance Pretty Hs.CommentBlockContent where
  pretty = \case
    Hs.Paragraph{..}      -> hsep
                           . map pretty
                           $ paragraphContent
    Hs.CodeBlock{..}      -> vcat
                           $ ["@"]
                          ++ map textToCtxDoc codeBlockLines
                          ++ ["@"]
    Hs.Verbatim{..}       -> ">" <+> textToCtxDoc verbatimContent
    Hs.Example{..}        -> ">>>" <+> textToCtxDoc exampleContent
    Hs.Property{..}       -> "prop>" <+> textToCtxDoc propertyContent
    Hs.ListItem{..}       ->
      let listMarker =
            case listItemType of
              Hs.BulletList -> "*"
              Hs.NumberedList n -> showToCtxDoc n >< "."
       in listMarker <+> vcat (map pretty listItemContent)
    Hs.DefinitionList{..} -> "["
                          >< pretty definitionListTerm
                          >< "]:"
                         <+> vcat (map pretty definitionListContent)
    Hs.Header{..}         -> string (replicate (fromEnum headerLevel) '=')
                         <+> (hsep $ map pretty headerContent)


instance Pretty Hs.CommentInlineContent where
  pretty = \case
    Hs.TextContent{..} -> textToCtxDoc textContent
    Hs.Monospace{..}   -> "@" >< hsep (map pretty monospaceContent) >< "@"
    Hs.Emph{..}        -> "/" >< hsep (map pretty emphContent) >< "/"
    Hs.Bold{..}        -> "__" >< hsep (map pretty boldContent) >< "__"
    Hs.Module{..}      -> "\"" >< textToCtxDoc moduleContent >< "\""
    Hs.Identifier{..}  -> "'" >< textToCtxDoc identifierContent >< "'"
    Hs.Type{..}        -> "t'" >< textToCtxDoc typeContent
    Hs.Link{..}        -> "[" >< hsep (map pretty linkLabel) >< "]"
                       >< "(" >< textToCtxDoc linkURL >< ")"
    Hs.URL{..}         -> "<" >< textToCtxDoc urlContent >< ">"
    Hs.Anchor{..}      -> "#" >< textToCtxDoc anchorContent >< "#"
    Hs.Math{..}        -> "\\[" >< vcat (map textToCtxDoc mathContent) >< "\\]"
    Hs.Metadata{..}    -> pretty metadataContent

instance Pretty Hs.CommentMeta where
  pretty Hs.Since{..} = "@since:" <+> textToCtxDoc sinceContent

{-------------------------------------------------------------------------------
  Declaration pretty-printing
-------------------------------------------------------------------------------}

instance Pretty SDecl where
  pretty = \case
    DVar Var {..} ->
      maybe empty (pretty . TopLevelComment) varComment
      $$
      (pretty varName <+> string "::" <+> pretty varType)
      $$
      fsep
        [ pretty varName <+> char '='
        , nest 2 $ pretty varExpr
        ]

    DInst Instance{..} ->
      let inst = hsep $
            ["instance", pretty (resolve instanceClass)]
              ++ map (prettyPrec 1) instanceArgs
              ++ ["where"]
          typs = flip map instanceTypes $ \(g, typArg, typSyn) -> nest 2 $ fsep
            [ "type" <+> ppUnqualBackendName (resolve g) <+> prettyPrec 1 typArg
                <+> char '='
            , nest 2 (pretty typSyn)
            ]
          decs = flip map instanceDecs $ \(name, expr) -> nest 2 $ fsep
            [ ppUnqualBackendName (resolve name) <+> char '='
            , nest 2 (pretty expr)
            ]
          prettyTopLevelComment = maybe empty (pretty . TopLevelComment) instanceComment
      in  vsep $ prettyTopLevelComment : inst : typs ++ decs

    DRecord Record{..} ->
      let d = hsep ["data", pretty dataType, char '=', pretty dataCon]
          prettyTopLevelComment = maybe empty (pretty . TopLevelComment) dataComment
      in  prettyTopLevelComment
       $$ (hang d 2 $
            vcat [ vlist '{' '}'
                     [   hsep [ pretty (fieldName f)
                              , "::"
                              , pretty (fieldType f)
                              ]
                     $$ prettyFieldComment
                     | f <- dataFields
                     , let prettyFieldComment = maybe empty (pretty . PartOfDeclarationComment) (fieldComment f)
                     ]
                 , nestedDeriving dataDeriv
                 ]
          )

    DEmptyData EmptyData{..} ->
      let prettyComment = maybe empty (pretty . TopLevelComment) emptyDataComment
       in  prettyComment
        $$ hsep ["data", pretty emptyDataName]

    DNewtype Newtype{..} ->
      let d = hsep ["newtype", pretty newtypeName, char '=', pretty newtypeCon]
          prettyComment = maybe empty (pretty . TopLevelComment) newtypeComment
          prettyFieldComment = maybe empty (pretty . PartOfDeclarationComment) (fieldComment newtypeField)
      in  prettyComment
       $$ (hang d 2 $ vcat [
             vlist '{' '}'
               [ hsep
                   [ pretty (fieldName newtypeField)
                   , "::"
                   , pretty (fieldType newtypeField)
                   ]
               $$ prettyFieldComment
               ]
           , nestedDeriving newtypeDeriv
           ])

    DForeignImport ForeignImport{..} ->
      -- Variable names here refer to the syntax of foreign declarations at
      -- <https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1540008.4>
      let -- TODO: We should escape special characters inside these import
          -- strings (at the very least quotes in filenames?)
          callconv, impent :: CtxDoc
          (callconv, impent) =
            case foreignImportCallConv of
              CallConvUserlandCAPI _ -> ("ccall",
                  string $ Text.unpack foreignImportOrigName
                )
              CallConvGhcCAPI header -> ("capi", hcat [
                  string header
                , string $ Text.unpack foreignImportOrigName
                ])
              CallConvGhcCCall style -> ("capi", hcat [
                  case style of
                    ImportAsValue -> ""
                    ImportAsPtr   -> "&"
                , string $ Text.unpack foreignImportOrigName
                ])

          prettyFunctionComment = maybe empty (pretty . TopLevelComment) foreignImportComment

      in   prettyFunctionComment
       $$  hsep [ "foreign import"
                , callconv
                , safety foreignImportSafety
                , "\"" >< impent >< "\""
                , pretty foreignImportName
                ]
       $$  nest 2 "::"
       <+> prettyForeignImportType foreignImportResultType
                                   foreignImportParameters

    DDerivingInstance DerivingInstance {..} -> maybe empty (pretty . TopLevelComment) derivingInstanceComment
                                            $$ "deriving" <+> strategy derivingInstanceStrategy
                                                          <+> "instance"
                                                          <+> pretty derivingInstanceType

    DPatternSynonym PatternSynonym {..} ->
      let prettyEnumConstantComment = maybe empty (pretty . TopLevelComment) patSynComment
       in vcat [ prettyEnumConstantComment
               , "pattern" <+> pretty patSynName <+> "::" <+> pretty patSynType
               , "pattern" <+> pretty patSynName <+> "=" <+> pretty patSynRHS
               ]

    DCSource src ->
      -- The single string literal is quite ugly but simple.
      "$(CAPI.addCSource" <+> fromString (show src) >< ")"

    DPragma p -> pragma p

-- | Nested deriving clauses (as part of a datatype declaration)
nestedDeriving :: [(Hs.Strategy ClosedType, [Global])] -> CtxDoc
nestedDeriving deriv = vcat [
      hsep [
          "deriving"
        , strategy s
        , hcat . concat $ [
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

pragma :: Pragma -> CtxDoc
pragma (NOINLINE n) = "{-# NOINLINE" <+> pretty n <+> "#-}"

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

prettyForeignImportType :: ResultType ClosedType -> [FunctionParameter] -> CtxDoc
prettyForeignImportType resultType params =
  case params of
    [] -> prettyResultType resultType
    _  -> prettyParams params
  where
    prettyParam FunctionParameter{..} =
         prettyType EmptyEnv 0 functionParameterType
      $$ maybe empty (pretty . PartOfDeclarationComment) functionParameterComment

    prettyResultType = \case
      NormalResultType t -> prettyType EmptyEnv 0 t
      HeapResultType t   ->
        let finalResType = TApp (TGlobal IO_type) (TGlobal (PrimType HsPrimUnit))
         in prettyType EmptyEnv 0 t
         $$ nest (-3) ("->" <+> prettyType EmptyEnv 0 finalResType)

    prettyParams []     = prettyResultType resultType
    prettyParams (p:ps) =
         prettyParam p
      $$ nest (-3) ("->" <+> prettyParams ps)

prettyType :: Env ctx CtxDoc -> Int -> SType ctx -> CtxDoc
prettyType env prec = \case
    TGlobal g -> pretty $ resolve g
    TCon n -> pretty n
    TLit n -> showToCtxDoc n
    TExt i _ctype -> pretty i
    TApp c x -> parensWhen (prec > 0) $
      prettyType env 1 c <+> prettyType env 1 x
    TFun a b -> parensWhen (prec > 0) $
      prettyType env 1 a <+> "->" <+> prettyType env 0 b
    TBound x -> lookupEnv x env
    TForall hints add ctxt body ->
      case add of
        AZ -> hsep (map (\ ct -> prettyType env 0 ct <+> "=> ") ctxt) >< prettyType env 0 body
        _  -> withFreshNames env add hints $ \env' params ->
          "forall" <+> hsep params >< "." <+>
          hsep (map (\ ct -> prettyType env' 0 ct <+> "=>") ctxt) <+> prettyType env' 0 body

prettyPrimType :: HsPrimType -> CtxDoc
prettyPrimType = prettyType EmptyEnv 0 . TGlobal . PrimType

{-------------------------------------------------------------------------------
  PatEpxr pretty-printing
-------------------------------------------------------------------------------}

instance Pretty PatExpr where
  prettyPrec = prettyPatExpr

prettyPatExpr :: Int -> PatExpr -> CtxDoc
prettyPatExpr prec = \case
    PELit i -> parensWhen (i < 0) $ showToCtxDoc i
    PEApps n ps -> parensWhen (prec > 3) $ pretty n <+> hsep (map (prettyPatExpr 4) ps)

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

    EIntegral i Nothing -> parensWhen (prec > 0 && i < 0) (showToCtxDoc i)
    EIntegral i (Just t) ->
      parens $ hcat [showToCtxDoc i, " :: ", prettyPrimType t]
    EChar (CharValue { charValue = ba, unicodeCodePoint = mbUnicode }) ->
      prettyExpr env 0 (EGlobal CharValue_fromAddr)
        <+> string str
        <+> string (show len)
        <+> case mbUnicode of { Nothing -> "Nothing"; Just c -> parens ("Just" <+> string (show c)) }
      where
        (str, len) = addrLiteral ba
    EString s -> showToCtxDoc s
    ECString bs ->
      -- Use unboxed Addr# literals to turn a string literal into a
      -- value of type CStringLen.
      let (str, len) = addrLiteral bs
      in parens $ hcat
        [ parens $ prettyExpr env 0 (EGlobal Ptr_constructor) <+> string str >< ", " >< string (show len)
        , " :: "
        , prettyPrimType HsPrimCStringLen
        ]

    EFloat f t -> parens $ hcat [
        if C.canBeRepresentedAsRational f then
          showToCtxDoc f
        else
          prettyExpr env prec $
            EApp (EGlobal CFloat_constructor) $
              EApp (EGlobal GHC_Float_castWord32ToFloat) $
                EIntegral (toInteger $ castFloatToWord32 f) (Just HsPrimCUInt)
      , " :: "
      , prettyPrimType t
      ]
    EDouble f t -> parens $ hcat [
        if C.canBeRepresentedAsRational f then
          showToCtxDoc f
        else
          prettyExpr env  prec $
            EApp (EGlobal CDouble_constructor) $
              EApp (EGlobal GHC_Float_castWord64ToDouble) $
                EIntegral (toInteger $ castDoubleToWord64 f) (Just HsPrimCULong)
      , " :: "
      , prettyPrimType t
      ]

    EApp f x -> parensWhen (prec > 3) $ prettyExpr env 3 f <+> prettyExpr env 4 x

    e@(EInfix op x y) -> case (prec, getInfixSpecialCase env e) of
      -- Handle special cases only at precedence 0.
      (0, Just ds) -> vcat ds
      -- Sub-expressions are aggresively parenthesized so that we do not have to
      -- worry about operator fixity/precedence.
      _otherwise ->
        parens $ hsep
          [ prettyExpr env 1 x
          , ppInfixBackendName (resolve op)
          , prettyExpr env 1 y
          ]

    ELam (NameHint hint) body -> withFreshName hint $ \x -> parensWhen (prec > 1) $ fsep
      [ char '\\' >< x <+> "->"
      , nest 2 $ prettyExpr (env :> x) 0 body
      ]

    EUnusedLam body -> parensWhen (prec > 1) $ fsep
      [ char '\\' >< "_" <+> "->"
      , nest 2 $ prettyExpr env 0 body
      ]

    ECase x alts -> vparensWhen (prec > 1) $
      if null alts
        then hsep ["case", prettyExpr env 0 x, "of", "{}"]
        else hang (hsep ["case", prettyExpr env 0 x, "of"]) 2 $ vcat
            [ withFreshNames env add hints $ \env' params ->

                let l = hsep $ pretty cnst : params ++ ["->"]
                in  ifFits l (fsep [l, nest 2 (prettyExpr env' 0 body)]) $
                    case unsnoc params of
                      Nothing -> fsep [l, nest 2 (prettyExpr env' 0 body)]
                      Just (lParams, rParam) -> vcat $
                          pretty cnst
                        : [ nest 2 param
                          | param <- lParams
                          ]
                        ++ [nest 2 (rParam <+> "->")]
                        ++ [nest 4 (prettyExpr env' 0 body)]

            | SAlt cnst add hints body <- alts
            ]

    ETup xs ->
      let ds = prettyExpr env 0 <$> xs
          l  = hlist '(' ')' ds
      in  ifFits l l $ vlist '(' ')' ds
    EList xs ->
      let ds = prettyExpr env 0 <$> xs
          l  = hlist '[' ']' ds
      in  ifFits l l $ vlist '[' ']' ds

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
withFreshNames env (AS a) (NameHint hint ::: hints) kont = withFreshName hint $ \name ->
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
      string . flip List.replicate ' ' . length . renderCtxDoc defaultContext

{-------------------------------------------------------------------------------
  HsName pretty-printing
-------------------------------------------------------------------------------}

instance Pretty (HsName ns) where
  pretty = string . Text.unpack . getHsName

{-------------------------------------------------------------------------------
  ResolvedName pretty-printing
-------------------------------------------------------------------------------}

-- | Pretty-print a 'ResolvedName' in prefix notation
--
-- Operators are parenthesized.
instance Pretty ResolvedName where
  pretty n@ResolvedName{..} =
    parensWhen (resolvedNameType == OperatorName) $ ppResolvedName n

-- | Pretty-print a 'ResolvedName'
--
-- This auxialary function pretty-prints without parenthesizing operators or
-- surrounding identifiers with backticks.
ppResolvedName :: ResolvedName -> CtxDoc
ppResolvedName ResolvedName{..} = case resolvedNameImport of
    Just (QualifiedHsImport HsImportModule{..}) ->
      let q = fromMaybe hsImportModuleName hsImportModuleAlias
      in  string $ q ++ '.' : resolvedNameString
    _otherwise -> string resolvedNameString

{-------------------------------------------------------------------------------
  BackendName pretty-printing
-------------------------------------------------------------------------------}

-- | Pretty-print a 'BackendName' in prefix notation
--
-- Operators are parenthesized.
instance Pretty BackendName where
  pretty = \case
    LocalBackendName nameType s ->
      parensWhen (nameType == OperatorName) $ string s
    ResolvedBackendName n -> pretty n

-- | Pretty-print a 'BackendName' unqualified
--
-- This is needed in instance declarations.
ppUnqualBackendName :: BackendName -> CtxDoc
ppUnqualBackendName = \case
    LocalBackendName nameType s ->
      parensWhen (nameType == OperatorName) $ string s
    ResolvedBackendName ResolvedName{..} ->
      parensWhen (resolvedNameType == OperatorName) $ string resolvedNameString

-- | Pretty-print a 'BackendName' in infix notation
--
-- Identifiers are surrounded by backticks.
ppInfixBackendName :: BackendName -> CtxDoc
ppInfixBackendName = \case
    LocalBackendName nameType s ->
      bticksWhen (nameType == IdentifierName) $ string s
    ResolvedBackendName n@ResolvedName{..} ->
      bticksWhen (resolvedNameType == IdentifierName) $ ppResolvedName n
  where
    bticksWhen :: Bool -> CtxDoc -> CtxDoc
    bticksWhen False d = d
    bticksWhen True  d = hcat [char '`', d, char '`']

{-------------------------------------------------------------------------------
  ExtIdentifier pretty-printing
-------------------------------------------------------------------------------}

instance Pretty HsModuleName where
  pretty = string . Text.unpack . getHsModuleName

instance Pretty HsIdentifier where
  pretty = string . Text.unpack . getHsIdentifier

instance Pretty ExtHsRef where
  pretty ExtHsRef{..} =
    hcat [pretty extHsRefModule, char '.', pretty extHsRefIdentifier]

{-------------------------------------------------------------------------------
  Auxiliary Functions
-------------------------------------------------------------------------------}

-- | In "Data.List" from @base-4.19.0.0@
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
{-# INLINABLE unsnoc #-}
