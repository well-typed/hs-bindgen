{-# OPTIONS_GHC -fno-warn-orphans #-}
module HsBindgen.Backend.PP.Render (
    -- * Rendering
    HsRenderOpts(..)
  , render
  , renderIO
  ) where

import Data.Maybe (maybeToList)
import Data.List qualified as List
import Data.Text qualified as Text
import GHC.Float (castFloatToWord32, castDoubleToWord64)
import System.IO

import HsBindgen.Imports
import HsBindgen.NameHint
import HsBindgen.SHs.AST
import HsBindgen.Backend.PP
import HsBindgen.Backend.PP.Render.Internal
import HsBindgen.Backend.PP.Translation
import HsBindgen.C.AST.Literal (canBeRepresentedAsRational)
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type (HsPrimType(..))

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
      "{-# LANGUAGE NoImplicitPrelude #-}"
    : hsep ["module", string hsModuleName, "where"]
    : vcat (map pretty hsModuleImports)
    : map pretty hsModuleDecls

{-------------------------------------------------------------------------------
  Import pretty-printing
-------------------------------------------------------------------------------}

resolve :: Global -> BackendName
resolve = ResolvedBackendName . resolveGlobal

instance Pretty ImportListItem where
  pretty = \case
    UnqualifiedImportListItem HsImport{..} ns -> hsep
      [ "import"
      , string hsImportModule
      , parens . hcat . List.intersperse ", " $ map pretty ns
      ]
    QualifiedImportListItem HsImport{..} -> case hsImportAlias of
      Just q -> hsep ["import qualified", string hsImportModule, "as", string q]
      Nothing -> hsep ["import qualified", string hsImportModule]

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
          [ "instance"
          , pretty (resolve instanceClass)
          , pretty instanceType
          , "where"
          ]
      : ( flip map instanceDecs $ \(name, expr) -> nest 2 $ fsep
            [ ppUnqualBackendName (resolve name) <+> char '='
            , nest 2 $ pretty expr
            ]
        )

    DRecord Record{..} ->
      let d = hsep ["data", pretty dataType, char '=', pretty dataCon]
      in  hang d 2 $ vlist '{' '}'
            [ hsep [pretty fld, "::", pretty typ]
            | (fld, typ) <- dataFields
            ]

    DEmptyData n ->
      hsep ["data", pretty n]

    DNewtype Newtype{..} ->
      let d = hsep ["newtype", pretty newtypeName, char '=', pretty newtypeCon]
      in  hang d 2 $ vlist '{' '}'
            [ hsep [pretty newtypeField, "::", pretty newtypeType]
            ]

    DForeignImport ForeignImport{..} ->
      let importStr = foreignImportHeader ++ " " ++ Text.unpack foreignImportOrigName
      in "foreign import capi safe" <+> fromString (show importStr) <+> pretty foreignImportName <+> "::" <+> pretty foreignImportType

    DDerivingNewtypeInstance t -> "deriving newtype instance" <+> pretty t

{-------------------------------------------------------------------------------
  Type pretty-printing
-------------------------------------------------------------------------------}

instance ctx ~ EmptyCtx => Pretty (SType ctx) where
  prettyPrec = prettyType EmptyEnv

prettyType :: Env ctx CtxDoc -> Int -> SType ctx -> CtxDoc
prettyType env prec = \case
    TGlobal g -> pretty $ resolve g
    TCon n -> pretty n
    TLit n -> pretty n
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

    EIntegral i _ -> showToCtxDoc i
    EFloat f
      | canBeRepresentedAsRational f
      -> showToCtxDoc f
      | otherwise
      ->
        prettyExpr env prec $
          EApp (EGlobal CFloat_constructor) $
            EApp (EGlobal GHC_Float_castWord32ToFloat) $
              EIntegral (toInteger $ castFloatToWord32 f) HsPrimCUInt
    EDouble f
      | canBeRepresentedAsRational f
      -> showToCtxDoc f
      | otherwise
      ->
        prettyExpr env  prec $
          EApp (EGlobal CDouble_constructor) $
            EApp (EGlobal GHC_Float_castWord64ToDouble) $
              EIntegral (toInteger $ castDoubleToWord64 f) HsPrimCULong

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

    ELam mPat body -> withFreshName mPat $ \x -> parensWhen (prec > 1) $ fsep
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

withFreshNames :: Env ctx CtxDoc -> Add n ctx ctx' -> Vec n NameHint -> (Env ctx' CtxDoc -> [CtxDoc] -> CtxDoc) -> CtxDoc
withFreshNames env AZ     _                kont = kont env []
withFreshNames env (AS a) (hint ::: hints) kont = withFreshName hint $ \name ->
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
ppResolvedName ResolvedName{..}
    | resolvedNameQualify =
        let q = fromMaybe (hsImportModule resolvedNameImport) $
              hsImportAlias resolvedNameImport
        in  string $ q ++ '.' : resolvedNameString
    | otherwise = string resolvedNameString

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
  Auxiliary Functions
-------------------------------------------------------------------------------}

-- | In "Data.List" from @base-4.19.0.0@
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
{-# INLINABLE unsnoc #-}
