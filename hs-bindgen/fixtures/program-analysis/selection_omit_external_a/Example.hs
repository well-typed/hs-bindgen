{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @UnrelatedDeclaration@

    __defined at:__ @program-analysis\/selection_omit_external_a.h:4:8@

    __exported by:__ @program-analysis\/selection_omit_external_a.h@
-}
data UnrelatedDeclaration = UnrelatedDeclaration
  { unrelatedDeclaration_m :: FC.CInt
    {- ^ __C declaration:__ @m@

         __defined at:__ @program-analysis\/selection_omit_external_a.h:5:7@

         __exported by:__ @program-analysis\/selection_omit_external_a.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable UnrelatedDeclaration where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure UnrelatedDeclaration
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"unrelatedDeclaration_m") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          UnrelatedDeclaration unrelatedDeclaration_m2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"unrelatedDeclaration_m") ptr0 unrelatedDeclaration_m2

instance HsBindgen.Runtime.HasCField.HasCField UnrelatedDeclaration "unrelatedDeclaration_m" where

  type CFieldType UnrelatedDeclaration "unrelatedDeclaration_m" =
    FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType UnrelatedDeclaration) "unrelatedDeclaration_m")
         ) => GHC.Records.HasField "unrelatedDeclaration_m" (Ptr.Ptr UnrelatedDeclaration) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"unrelatedDeclaration_m")
