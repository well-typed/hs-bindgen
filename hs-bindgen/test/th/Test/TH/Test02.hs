{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.TH.Test02 where

import Optics ((%), (&), (.~))

import HsBindgen.Runtime.Prelude qualified

import HsBindgen.TH

let cfg :: Config
    cfg = def & #clang % #extraIncludeDirs .~ [
              Pkg "examples"
            ]
 in withHsBindgen cfg def $
      hashInclude "test_02.h"
