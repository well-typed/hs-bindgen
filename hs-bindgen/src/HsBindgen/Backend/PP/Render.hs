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
      hsep ["module", string moduleName, "where"]
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
        hsep ["instance", pretty instanceClass, pretty instanceType, "where"]
      : ( flip map instanceDecs $ \(name, expr) -> nest 2 $ fsep
            [ pretty name <+> char '='
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

    e@(EInfix _op x EInfix{}) | prec <= 2 ->
      hang (prettyPrec 1 x) 2 $ vcat (getInfixes e)
    EInfix op x y
      | prec > 2 -> parens $
          hsep [prettyPrec 1 x, ppInfix (resolve BE op), prettyPrec 1 y]
      | otherwise -> fsep
          [ prettyPrec 1 x
          , nest 2 $ ppInfix (resolve BE op) <+> prettyPrec 1 y
          ]

    ELam mPat body -> parensWhen (prec > 1) $ fsep
      [ char '\\' >< maybe "_" (pretty . getFresh) mPat <+> "->"
      , nest 2 $ pretty body
      ]

    ECase x ms -> hang (hsep ["case", pretty x, "of"]) 2 $
      vcat . flip map ms $ \(cnst, params, body) ->
        let l = hsep $
              pretty cnst : map (prettyPrec 3 . getFresh) params ++ ["->"]
        in  ifFits l (fsep [l, nest 2 (pretty body)]) $
              case unsnoc params of
                Nothing -> fsep [l, nest 2 (pretty body)]
                Just (lParams, rParam) -> vcat $
                    pretty cnst
                  : [nest 2 (prettyPrec 3 (getFresh param)) | param <- lParams]
                  ++ [nest 2 ((prettyPrec 3 (getFresh rParam)) <+> "->")]
                  ++ [nest 4 (pretty body)]

    EInj x -> prettyPrec prec x

getInfixes :: SExpr BE -> [CtxDoc]
getInfixes = \case
    EInfix op _x y@(EInfix _op x _y) ->
      ppInfix (resolve BE op) <+> prettyPrec 1 x : getInfixes y
    EInfix op _x y -> [ppInfix (resolve BE op) <+> prettyPrec 1 y]
    _otherwise -> []

{-------------------------------------------------------------------------------
  Name instances
-------------------------------------------------------------------------------}

instance Pretty Global where
  pretty = pretty . resolve BE

instance Pretty ResolvedName where
  pretty = \case
    ResolvedIdent s    -> string s
    ResolvedOperator s -> parens $ string s

instance Pretty (HsName ns) where
  pretty = string . Text.unpack . getHsName

ppInfix :: ResolvedName -> CtxDoc
ppInfix = \case
    ResolvedIdent s    -> hcat [char '`', string s, char '`']
    ResolvedOperator s -> string s

{-------------------------------------------------------------------------------
  Auxiliary Functions
-------------------------------------------------------------------------------}

-- | In "Data.List" from @base-4.19.0.0@
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
{-# INLINABLE unsnoc #-}
