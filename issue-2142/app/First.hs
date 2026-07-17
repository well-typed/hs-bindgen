module First where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Optics ((%), (&), (.~))

import HsBindgen.Runtime.LibC qualified
import HsBindgen.TH

let cfg :: Config
    cfg = def
      & #clang % #extraIncludeDirs .~ [Pkg "cbits"]
    cfgTh :: ConfigTH
    cfgTh = def
 in withHsBindgen cfg cfgTh $
      hashInclude "first.h"

initialize :: IO FirstHandle
initialize = alloca $ \pHandle -> do
  initFirstHandle pHandle
  peek pHandle
