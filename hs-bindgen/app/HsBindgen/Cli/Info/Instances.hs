-- | @hs-bindgen-cli info instances@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Info.Instances qualified as Instances
module HsBindgen.Cli.Info.Instances (
    -- * CLI help
    info
    -- * Execution
  , exec
  ) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Options.Applicative hiding (info)

import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "List supported instances and strategies"

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: IO ()
exec = mapM_ putStrLn (aux def)
  where
    aux :: Inst.SupportedInstances -> [String]
    aux supportedInstances =
         auxKind "struct"  supportedInstances.struct
      ++ auxKind "union"   supportedInstances.union
      ++ auxKind "enum"    supportedInstances.enum
      ++ auxKind "typedef" supportedInstances.typedef

    auxKind :: String -> Map Inst.TypeClass Inst.SupportedStrategies -> [String]
    auxKind kind classMap =
        ("* `" ++ kind ++ "`")
      : map (indent . uncurry auxClass) (Map.toAscList classMap)

    auxClass :: Inst.TypeClass -> Inst.SupportedStrategies -> String
    auxClass clss supportedStrategies = "* `" ++ show clss ++ "`: "
      ++  List.intercalate ", "
            ( map
                (auxStrategy supportedStrategies.defStrategy)
                (Set.toAscList supportedStrategies.strategies)
            )

    auxStrategy :: Maybe Inst.Strategy -> Inst.Strategy -> String
    auxStrategy mDefStrat strat = '`' : show strat ++ "`" ++ case mDefStrat of
      Just defStrat | strat == defStrat -> " (default)"
      _otherwise                        -> ""

    indent :: String -> String
    indent = ("    " ++)
