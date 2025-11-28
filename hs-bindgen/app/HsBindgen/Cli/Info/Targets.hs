-- | @hs-bindgen-cli info targets@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Info.Targets qualified as Targets
module HsBindgen.Cli.Info.Targets (
    -- * CLI help
    info
    -- * Execution
  , exec
  ) where

import Data.List qualified as List
import Options.Applicative hiding (info)

import HsBindgen.Config.ClangArgs

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "List supported targets"

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: IO ()
exec = mapM_ putStrLn $
      formatRow tableHeader
    : List.replicate (sum lens - 3) '-'
    : map formatRow tableRows
  where
    tableHeader :: [String]
    tableHeader = ["Target", "Processor", "OS", "C library"]

    targetRow :: Target -> [String]
    targetRow t = targetTriple t : case t of
      Target_Linux_GNU_X86_64    -> ["Intel/AMD 64-bit", "Linux",   "glibc"]
      Target_Linux_GNU_AArch64   -> ["ARM 64-bit",       "Linux",   "glibc"]
      Target_Linux_Musl_X86_64   -> ["Intel/AMD 64-bit", "Linux",   "Musl"]
      Target_Linux_Musl_AArch64  -> ["ARM 64-bit",       "Linux",   "Musl"]
      Target_Windows_MSVC_X86_64 -> ["Intel/AMD 64-bit", "Windows", "MSVC"]
      Target_Windows_GNU_X86_64  -> ["Intel/AMD 64-bit", "Windows", "glibc"]
      Target_Darwin_X86_64       -> ["Intel/AMD 64-bit", "macOS",   ""]
      Target_Darwin_AArch64      -> ["Apple/ARM 64-bit", "macOS",   ""]

    tableRows :: [[String]]
    tableRows = map targetRow [minBound..]

    lens :: [Int]
    lens =
      (+ 3) . maximum . map length <$> List.transpose (tableHeader : tableRows)

    formatRow :: [String] -> String
    formatRow ss = List.dropWhileEnd (== ' ') $
      concatMap (uncurry formatCol) (zip ss lens)

    formatCol :: String -> Int -> String
    formatCol s n = take n $ s ++ List.repeat ' '
