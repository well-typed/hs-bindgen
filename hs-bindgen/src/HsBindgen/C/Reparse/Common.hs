{-# LANGUAGE OverloadedStrings #-}

-- | Common parsers (for values throughout the AST)
module HsBindgen.C.Reparse.Common (
    reparseName
  , reparseAttribute
  ) where

import Control.Monad
import Text.Parsec hiding (token)

import HsBindgen.C.AST
import HsBindgen.C.Reparse.Infra
import HsBindgen.Clang.Core
import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Identifiers
-------------------------------------------------------------------------------}

reparseName :: Reparse (MultiLoc, CName)
reparseName = token $ \t -> do
    guard $ fromSimpleEnum (tokenKind t) == Right CXToken_Identifier
    return (
        rangeStart $ tokenExtent t
      , CName $ getTokenSpelling (tokenSpelling t)
      )

{-------------------------------------------------------------------------------
  Attributes

  We don't really parse attributes, but just skip over them.
-------------------------------------------------------------------------------}

-- | Parse attribute
--
-- > __attribute__ (( .. ))
reparseAttribute :: Reparse Attribute
reparseAttribute = fmap Attribute $ do
    exact CXToken_Keyword "__attribute__"
    doubleOpenParens *> anythingMatchingBrackets <* doubleCloseParens
  where
    doubleOpenParens, doubleCloseParens :: Reparse ()
    doubleOpenParens  = (punctuation "(" >> punctuation "(") <?> "(("
    doubleCloseParens = (punctuation ")" >> punctuation ")") <?> "))"

