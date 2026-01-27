{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.Block
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.HasFFIType
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (IO)

{-| __C declaration:__ @Toggle@

    __defined at:__ @edge-cases\/iterator.h 3:16@

    __exported by:__ @edge-cases\/iterator.h@
-}
newtype Toggle = Toggle
  { un_Toggle :: HsBindgen.Runtime.Block.Block (IO FC.CBool)
  }
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Toggle) "un_Toggle")
         ) => GHC.Records.HasField "un_Toggle" (Ptr.Ptr Toggle) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Toggle")

instance HsBindgen.Runtime.HasCField.HasCField Toggle "un_Toggle" where

  type CFieldType Toggle "un_Toggle" =
    HsBindgen.Runtime.Block.Block (IO FC.CBool)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @Counter@

    __defined at:__ @edge-cases\/iterator.h 10:14@

    __exported by:__ @edge-cases\/iterator.h@
-}
newtype Counter = Counter
  { un_Counter :: HsBindgen.Runtime.Block.Block (IO FC.CInt)
  }
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Counter) "un_Counter")
         ) => GHC.Records.HasField "un_Counter" (Ptr.Ptr Counter) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Counter")

instance HsBindgen.Runtime.HasCField.HasCField Counter "un_Counter" where

  type CFieldType Counter "un_Counter" =
    HsBindgen.Runtime.Block.Block (IO FC.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @VarCounter@

    __defined at:__ @edge-cases\/iterator.h 17:14@

    __exported by:__ @edge-cases\/iterator.h@
-}
newtype VarCounter = VarCounter
  { un_VarCounter :: HsBindgen.Runtime.Block.Block (FC.CInt -> IO FC.CInt)
  }
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType VarCounter) "un_VarCounter")
         ) => GHC.Records.HasField "un_VarCounter" (Ptr.Ptr VarCounter) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_VarCounter")

instance HsBindgen.Runtime.HasCField.HasCField VarCounter "un_VarCounter" where

  type CFieldType VarCounter "un_VarCounter" =
    HsBindgen.Runtime.Block.Block (FC.CInt -> IO FC.CInt)

  offset# = \_ -> \_ -> 0
