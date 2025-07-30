-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Test.TH.Test01 where

import HsBindgen.Runtime.Prelude qualified
import HsBindgen.TH

let opts = def { extraSystemIncludeDirs = [ RelativeToPkgRoot "examples" ] }
 in withHsBindgen opts $ hashInclude $ Quote "test_01.h"
