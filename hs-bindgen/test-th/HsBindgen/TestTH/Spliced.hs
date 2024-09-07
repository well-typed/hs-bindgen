{-# OPTIONS_GHC -Wno-orphans -ddump-splices -ddump-to-file #-}

{-# LANGUAGE TemplateHaskell #-}

module HsBindgen.TestTH.Spliced where

import HsBindgen.Backend.TH.Translation
import HsBindgen.TestTH.Examples

translateHs decls