{-# LANGUAGE MagicHash #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsBindgen.Backend.PP.Render (
    -- * Rendering
    HsRenderOpts(..)
  , render
  , renderIO
  ) where

import Data.Maybe (maybeToList)
import Data.Char qualified
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Word
import GHC.Float (castFloatToWord32, castDoubleToWord64)
import GHC.Exts (Int(..), sizeofByteArray#)
import GHC.Exts qualified as IsList(IsList(..))
import System.IO
import Numeric (showHex)

import HsBindgen.Backend.PP.Names
import HsBindgen.Backend.PP.Translation
import HsBindgen.C.AST.Literal (canBeRepresentedAsRational)
import HsBindgen.ExtBindings
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type (HsPrimType(..))
import HsBindgen.Imports
import HsBindgen.NameHint
import HsBindgen.SHs.AST
import Text.SimplePrettyPrint

import C.Char (CharValue(..))
import DeBruijn (EmptyCtx, Env (..), lookupEnv, Add (..))

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

-- | Rendering options
newtype HsRenderOpts = HsRenderOpts {
      hsLineLength :: Int
    }
  deriving stock (Show)

instance Default HsRenderOpts where
  def = HsRenderOpts {
      hsLineLength = 80
    }

-- | Render generated bindings
render :: HsRenderOpts -> HsModule -> String
render HsRenderOpts{..} = renderPretty (mkContext hsLineLength)

-- | Write rendered bindings to the specified file (or @stdout@)
renderIO :: HsRenderOpts -> Maybe FilePath -> HsModule -> IO ()
renderIO opts Nothing   modl = putStrLn $ render opts modl
renderIO opts (Just fp) modl = withFile fp WriteMode $ \h ->
    hPutStrLn h $ render opts modl

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
  pretty ghcPragma = hsep ["{-#", string ghcPragma, "#-}"]

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
  Declaration pretty-printing
-------------------------------------------------------------------------------}

instance Pretty SDecl where
  pretty = \case
    DVar name mbTy expr ->
      fsep
        [ pretty name <+> string "::" <+> pretty ty
        | ty <- maybeToList mbTy ]
      $$
      fsep
        [ pretty name <+> char '='
        , nest 2 $ pretty expr
        ]

    DInst Instance{..} -> vsep $
        hsep
          ([ "instance"
          , pretty (resolve instanceClass)
          ] ++
          map pretty instanceArgs ++
          [ "where"
          ])
      : ( flip map instanceDecs $ \(name, expr) -> nest 2 $ fsep
            [ ppUnqualBackendName (resolve name) <+> char '='
            , nest 2 $ pretty expr
            ]
        )

    DRecord Record{..} ->
      let d = hsep ["data", pretty dataType, char '=', pretty dataCon]
      in  hang d 2 $ vlist '{' '}'
            [ hsep [pretty (fieldName f), "::", pretty (fieldType f)]
            | f <- dataFields
            ]

    DEmptyData d ->
      hsep ["data", pretty (emptyDataName d)]

    DNewtype Newtype{..} ->
      let d = hsep ["newtype", pretty newtypeName, char '=', pretty newtypeCon]
      in  hang d 2 $ vlist '{' '}'
            [ hsep
                [ pretty (fieldName newtypeField)
                , "::"
                , pretty (fieldType newtypeField)
                ]
            ]

    DForeignImport ForeignImport{..} ->
      let importStr = foreignImportHeader ++ " " ++ Text.unpack foreignImportOrigName
      in hsep [ "foreign import capi safe"
              , "\"" >< string importStr >< "\""
              , pretty foreignImportName
              , "::"
              , pretty foreignImportType
              ]

    DDerivingInstance s t -> "deriving" <+> strategy s <+> "instance" <+> pretty t

    DPatternSynonym PatternSynonym {..} -> vcat
      [ "pattern" <+> pretty patSynName <+> "::" <+> pretty patSynType
      , "pattern" <+> pretty patSynName <+> "=" <+> pretty patSynRHS
      ]

strategy :: Hs.Strategy ClosedType -> CtxDoc
strategy Hs.DeriveNewtype  = "newtype"
strategy Hs.DeriveStock    = "stock"
strategy (Hs.DeriveVia ty) = "via" <+> pretty ty

{-------------------------------------------------------------------------------
  Type pretty-printing
-------------------------------------------------------------------------------}

instance ctx ~ EmptyCtx => Pretty (SType ctx) where
  prettyPrec = prettyType EmptyEnv

prettyType :: Env ctx CtxDoc -> Int -> SType ctx -> CtxDoc
prettyType env prec = \case
    TGlobal g -> pretty $ resolve g
    TCon n -> pretty n
    TLit n -> showToCtxDoc n
    TExt i -> pretty i
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

    EIntegral i Nothing  -> showToCtxDoc i
    EIntegral i (Just t) -> parens $ hcat [
          parensWhen (i < 0) (showToCtxDoc i)
        , " :: "
        , prettyPrimType t
        ]
    EChar (CharValue { charValue = ba, unicodeCodePoint = mbUnicode }) ->
      prettyExpr env 0 (EGlobal CharValue_fromAddr)
        <+> string str
        <+> string (show len)
        <+> case mbUnicode of { Nothing -> "Nothing"; Just c -> parens ("Just" <+> string (show c)) }
      where
        (str, len) = addrLiteral ba
    EString bs ->
      -- Use unboxed Addr# literals to turn a string literal into a
      -- value of type CStringLen.
      let (str, len) = addrLiteral bs
      in parens $ hcat
        [ parens $ prettyExpr env 0 (EGlobal Ptr_constructor) <+> string str >< ", " >< string (show len)
        , " :: "
        , prettyPrimType HsPrimCStringLen
        ]

    EFloat f t -> parens $ hcat [
        if canBeRepresentedAsRational f then
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
        if canBeRepresentedAsRational f then
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

instance Pretty ExtIdentifier where
  pretty ExtIdentifier{..} =
    hcat [pretty extIdentifierModule, char '.', pretty extIdentifierIdentifier]

{-------------------------------------------------------------------------------
  Auxiliary Functions
-------------------------------------------------------------------------------}

-- | In "Data.List" from @base-4.19.0.0@
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
{-# INLINABLE unsnoc #-}
