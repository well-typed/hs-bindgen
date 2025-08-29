{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}

{-# LANGUAGE TemplateHaskell #-}

module Test.TH.Test01 where

import HsBindgen.Runtime.Prelude qualified
import HsBindgen.TH

let opts = def { extraIncludeDirs = [ RelativeToPkgRoot "examples" ] }
 in withHsBindgen opts $ hashInclude "test_01.h"
