-- | Names
module HsBindgen.Frontend.Macro.Parse.Name (
    parseName
  , parseLocName
  ) where

import Clang.Enum.Simple
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import HsBindgen.Frontend.Macro.Parse.Infra
import HsBindgen.Frontend.Naming (Name(..))
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Identifiers
-------------------------------------------------------------------------------}

parseName :: Parser Name
parseName = snd <$> parseLocName

parseLocName :: Parser (MultiLoc, Name)
parseLocName = token $ \t -> do
    let spelling = getTokenSpelling (tokenSpelling t)
    let ki = fromSimpleEnum (tokenKind t)
    -- bool become keyword in later LLVMs (not in 14, surely in 16)
    guard $ ki == Right CXToken_Identifier
    return (
        rangeStart $ tokenExtent t
      , Name spelling
      )
