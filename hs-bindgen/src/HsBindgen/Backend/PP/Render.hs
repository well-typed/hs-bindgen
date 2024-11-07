{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsBindgen.Backend.PP.Render (
    -- * Rendering
    HsRenderOpts(..)
  , render
  , renderIO
  ) where

import Data.Default
import Data.List qualified as List
import Data.Maybe
import Data.Text qualified as Text
import System.IO

import HsBindgen.Backend.Common
import HsBindgen.Backend.PP
import HsBindgen.Backend.PP.Render.Internal
import HsBindgen.Backend.PP.Translation
import HsBindgen.Hs.AST.Name

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

instance Pretty (SDecl BE) where
  pretty = \case
    DVar name expr -> fsep
      [ pretty name <+> char '='
      , nest 2 $ pretty expr
      ]

    DInst Instance{..} -> vsep $
        hsep
          [ "instance"
          , pretty (resolve BE instanceClass)
          , pretty instanceType
          , "where"
          ]
      : ( flip map instanceDecs $ \(name, expr) -> nest 2 $ fsep
            [ ppUnqualBackendName (resolve BE name) <+> char '='
            , nest 2 $ pretty expr
            ]
        )

    DRecord Record{..} ->
      let d = hsep ["data", pretty dataType, char '=', pretty dataCon]
      in  hang d 2 $ vlist '{' '}'
            [ hsep [pretty fld, "::", pretty typ]
            | (fld, typ) <- dataFields
            ]

    DNewtype Newtype{..} ->
      let d = hsep ["newtype", pretty newtypeName, char '=', pretty newtypeCon]
      in  hang d 2 $ vlist '{' '}'
            [ hsep [pretty newtypeField, "::", pretty newtypeType]
            ]

{-------------------------------------------------------------------------------
  Type pretty-printing
-------------------------------------------------------------------------------}

instance Pretty (SType BE) where
  prettyPrec prec = \case
    TGlobal g -> pretty $ resolve BE g

    TCon n -> pretty n

    TApp c x -> parensWhen (prec > 0) $ prettyPrec 1 c <+> prettyPrec 1 x

{-------------------------------------------------------------------------------
  Expression pretty-printing
-------------------------------------------------------------------------------}

instance Pretty (SExpr BE) where
  prettyPrec prec = \case
    EGlobal g -> pretty $ resolve BE g

    EVar x -> pretty $ getFresh x

    ECon n -> pretty n

    EInt i -> showToCtxDoc i

    EApp f x -> parensWhen (prec > 3) $ prettyPrec 3 f <+> prettyPrec 4 x

    -- aggressively parenthesize so that we do not have to worry about operator
    -- fixity and precedence
    EInfix op x y -> parensWhen (prec > 0) $
      prettyPrec 1 x <+> ppInfixBackendName (resolve BE op) <+> prettyPrec 1 y

    ELam mPat body -> parensWhen (prec > 1) $ fsep
      [ char '\\' >< maybe "_" (pretty . getFresh) mPat <+> "->"
      , nest 2 $ pretty body
      ]

    ECase x ms -> vparensWhen (prec > 1) $
      if null ms
        then hsep ["case", pretty x, "of", "{}"]
        else hang (hsep ["case", pretty x, "of"]) 2 $
          vcat . flip map ms $ \(cnst, params, body) ->
            let l = hsep $
                  pretty cnst : map (prettyPrec 3 . getFresh) params ++ ["->"]
            in  ifFits l (fsep [l, nest 2 (pretty body)]) $
                  case unsnoc params of
                    Nothing -> fsep [l, nest 2 (pretty body)]
                    Just (lParams, rParam) -> vcat $
                        pretty cnst
                      : [ nest 2 (prettyPrec 3 (getFresh param))
                        | param <- lParams
                        ]
                      ++ [nest 2 (prettyPrec 3 (getFresh rParam) <+> "->")]
                      ++ [nest 4 (pretty body)]

    EInj x -> prettyPrec prec x

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
