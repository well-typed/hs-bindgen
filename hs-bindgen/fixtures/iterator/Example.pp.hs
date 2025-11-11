{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Block
import Prelude (IO)

{-| __C declaration:__ @Toggle@

    __defined at:__ @iterator.h:3:16@

    __exported by:__ @iterator.h@
-}
newtype Toggle = Toggle
  { un_Toggle :: HsBindgen.Runtime.Block.Block (IO FC.CBool)
  }

{-| __C declaration:__ @Counter@

    __defined at:__ @iterator.h:10:14@

    __exported by:__ @iterator.h@
-}
newtype Counter = Counter
  { un_Counter :: HsBindgen.Runtime.Block.Block (IO FC.CInt)
  }

{-| __C declaration:__ @VarCounter@

    __defined at:__ @iterator.h:17:14@

    __exported by:__ @iterator.h@
-}
newtype VarCounter = VarCounter
  { un_VarCounter :: HsBindgen.Runtime.Block.Block (FC.CInt -> IO FC.CInt)
  }
