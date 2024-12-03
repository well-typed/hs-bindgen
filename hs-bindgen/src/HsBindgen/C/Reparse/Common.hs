-- | Common parsers (for values throughout the AST)
module HsBindgen.C.Reparse.Common (
    reparseName
  , reparseLocName
  , reparseAttribute
  ) where

import Text.Parsec hiding (token)

import HsBindgen.Imports
import HsBindgen.C.AST
import HsBindgen.C.Reparse.Infra
import HsBindgen.Clang.LowLevel.Core
import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Identifiers
-------------------------------------------------------------------------------}

reparseName :: Reparse CName
reparseName = snd <$> reparseLocName

reparseLocName :: Reparse (MultiLoc, CName)
reparseLocName = token $ \t -> do
    let spelling = getTokenSpelling (tokenSpelling t)
    let ki = fromSimpleEnum (tokenKind t)
    -- bool become keyword in later LLVMs (not in 14, surely in 16)
    guard $ ki == Right CXToken_Identifier
    return (
        rangeStart $ tokenExtent t
      , CName spelling
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

