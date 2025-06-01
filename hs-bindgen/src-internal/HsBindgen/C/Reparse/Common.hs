-- | Common parsers (for values throughout the AST)
module HsBindgen.C.Reparse.Common (
    reparseName
  , reparseLocName
  , manyTillLookahead
  , getRemaining
  ) where

import Text.Parsec hiding (token)
import Data.Text qualified as Text

import Clang.Enum.Simple
import Clang.LowLevel.Core
import Clang.HighLevel.Types

import HsBindgen.C.Reparse.Infra
import HsBindgen.Frontend.AST.Internal (CName(..))
import HsBindgen.Imports

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
  Utils
-------------------------------------------------------------------------------}

-- | Like 'Parsec.manyTill', but works when the two parsers overlap.
manyTillLookahead
  :: (Stream s m t, Show t)
  => ParsecT s u m a
  -> ParsecT s u m e
  -> ParsecT s u m ([a], e)
manyTillLookahead p end = go
  where
    go = choice
      [ try $
        do { e <- end
           ; eof <|> notFollowedBy (void p <|> void end)
           ; return ([], e)
           }
      , do { a <- p; (as, e) <- go ; return (a:as, e)}
      ]

-- | Debug utility function: get the remaining input stream.
getRemaining :: Reparse String
getRemaining = do
  remaining <- getInput
  return $ Text.unpack $ Text.concat $
    map (getTokenSpelling . tokenSpelling) remaining
