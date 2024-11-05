{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsBindgen.Backend.PP.Render (
    HsRenderOpts(..)
  , render
  , renderIO
  ) where

import Data.Default
import Data.Text qualified as Text
import System.IO

import HsBindgen.Backend.Common
import HsBindgen.Backend.PP
import HsBindgen.Backend.PP.Render.Internal
import HsBindgen.Backend.PP.Translation
import HsBindgen.Hs.AST.Name

{-------------------------------------------------------------------------------
  Options
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

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

-- | Render generated bindings
render :: HsRenderOpts -> Module -> String
render HsRenderOpts{..} = renderPretty (mkContext hsLineLength)

-- | Write rendered bindings to the specified file (or @stdout@)
renderIO :: HsRenderOpts -> Maybe FilePath -> Module -> IO ()
renderIO opts Nothing   modl = putStrLn $ render opts modl
renderIO opts (Just fp) modl = withFile fp WriteMode $ \h ->
    hPutStrLn h $ render opts modl

{-------------------------------------------------------------------------------
  Module instance
-------------------------------------------------------------------------------}

instance Pretty Module where
  pretty Module{..} = vsep $
      "{-# LANGUAGE NoImplicitPrelude #-}"
    : hsep ["module", string moduleName, "where"]
    : vcat ["import qualified" <+> pretty im | im <- moduleImports]
    : map pretty moduleDecls

{-------------------------------------------------------------------------------
  SDecl instance
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
            [ ppUnqual (resolve BE name) <+> char '='
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
  SType instance
-------------------------------------------------------------------------------}

instance Pretty (SType BE) where
  prettyPrec prec = \case
    TGlobal g -> pretty $ resolve BE g

    TCon n -> pretty n

    TApp c x -> parensWhen (prec > 0) $ prettyPrec 1 c <+> prettyPrec 1 x

{-------------------------------------------------------------------------------
  SExpr instance
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
      prettyPrec 1 x <+> ppInfix (resolve BE op) <+> prettyPrec 1 y

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
  Name instances
-------------------------------------------------------------------------------}

instance Pretty (HsName ns) where
  pretty = string . Text.unpack . getHsName

instance Pretty ResolvedName where
  pretty ResolvedName{..} =
    parensWhen (resolvedNameType == ResolvedNameOperator) . string $
      case resolvedNameQualifier of
        Just (QualifiedImport q) -> q ++ '.' : resolvedNameString
        Nothing                  -> resolvedNameString

ppInfix :: ResolvedName -> CtxDoc
ppInfix ResolvedName{..} =
    bticksWhen (resolvedNameType == ResolvedNameIdentifier) . string $
      case resolvedNameQualifier of
        Just (QualifiedImport q) -> q ++ '.' : resolvedNameString
        Nothing                  -> resolvedNameString
  where
    bticksWhen :: Bool -> CtxDoc -> CtxDoc
    bticksWhen False d = d
    bticksWhen True  d = hcat [char '`', d, char '`']

ppUnqual :: ResolvedName -> CtxDoc
ppUnqual ResolvedName{..} =
    parensWhen (resolvedNameType == ResolvedNameOperator) $
      string resolvedNameString

{-------------------------------------------------------------------------------
  Import instance
-------------------------------------------------------------------------------}

instance Pretty QualifiedImport where
  pretty (QualifiedImport s) = string s

{-------------------------------------------------------------------------------
  Auxiliary Functions
-------------------------------------------------------------------------------}

-- | In "Data.List" from @base-4.19.0.0@
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
{-# INLINABLE unsnoc #-}
