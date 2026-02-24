{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsBindgen.Backend.HsModule.Pretty.Common (
    -- * Names
    prettyResolvedNamePlain

    -- * Tuples
  , prettyBoxedTuple
  , prettyUnboxedTuple

    -- * Helpers
  , withFreshNames
  , panicWith
  ) where

import Data.Text qualified as Text
import DeBruijn (Add (..), Env (..))
import Text.SimplePrettyPrint (CtxDoc, Pretty (..))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.HsModule.Names
import HsBindgen.Backend.SHs.AST
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint

{-------------------------------------------------------------------------------
  Names
-------------------------------------------------------------------------------}

instance Pretty (Hs.Name ns) where
  pretty = PP.text . Hs.getName

-- | Pretty-print a 'ResolvedName' in prefix notation
--
-- Operators are parenthesized.
instance Pretty ResolvedName where
  pretty resolved =
    PP.parensWhen (resolved.typ == OperatorName) $ prettyResolvedNamePlain resolved

-- | Pretty-print a 'ResolvedName'
--
-- This auxialary function pretty-prints without parenthesizing operators or
-- surrounding identifiers with backticks.
prettyResolvedNamePlain :: ResolvedName -> CtxDoc
prettyResolvedNamePlain resolved =
    case resolved.hsImport of
      Hs.QualifiedImport name alias ->
        let q = fromMaybe (Hs.moduleNameToString name) alias
        in  PP.string $ q ++ '.' : resolved.string
      _otherwise ->
        PP.string resolved.string

{-------------------------------------------------------------------------------
  External references
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
  Tuples
-------------------------------------------------------------------------------}

prettyTupleWith :: String -> String -> Plus2 -> [CtxDoc] -> CtxDoc
prettyTupleWith pre pos n decls = case compare arity nDecls of
    LT ->
      panicPure $ mconcat [
          "Too many declarations ("
        , show nDecls
        , ") for "
        , show arity ++ "-tuple"
        ]
    _otherwise ->
      let nMissing  = arity - nDecls
          fakeDecls = decls ++ replicate nMissing ""
          lsOneLn   = PP.hlist pre pos fakeDecls
          lsMulLn   = PP.vlist pre pos fakeDecls
      in  PP.ifFits lsOneLn lsOneLn lsMulLn
  where
    arity :: Int
    arity = fromIntegral $ applyPlus2 n

    nDecls :: Int
    nDecls = length decls

prettyBoxedTuple, prettyUnboxedTuple :: Plus2 -> [CtxDoc] -> CtxDoc
prettyBoxedTuple   = prettyTupleWith "(" ")"
prettyUnboxedTuple = prettyTupleWith "(#" "#)"

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

withFreshNames ::
     Env ctx CtxDoc
  -> Add n ctx ctx'
  -> Vec n NameHint
  -> (Env ctx' CtxDoc -> [CtxDoc] -> CtxDoc)
  -> CtxDoc
withFreshNames env AZ     _                         kont = kont env []
withFreshNames env (AS a) (NameHint hint ::: hints) kont = PP.withFreshName hint $ \name ->
    withFreshNames env a hints $ \env' names -> kont (env' :> name) (name : names)

panicWith :: Show a => String -> a -> b
panicWith msg x = panicPure $ msg ++ "; " ++ show x
