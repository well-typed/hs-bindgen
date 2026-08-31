{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Widget_legacy_t(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified M

{-| __C declaration:__ @widget_legacy_t@

    __defined at:__ @binding-specs\/include_spelling\/sub\/consumer.h 3:25@

    __exported by:__ @binding-specs\/include_spelling\/sub\/consumer.h@
-}
newtype Widget_legacy_t = Widget_legacy_t
  { unwrapWidget_legacy_t :: M.WidgetT
  }
  deriving stock (BG.Generic)

instance ( ty ~ M.WidgetT
         ) => BG.CompatHasField.HasField "unwrapWidget_legacy_t" Widget_legacy_t ty where

  hasField =
    \x0 ->
      ( \y1 -> Widget_legacy_t {unwrapWidget_legacy_t = y1}
      , BG.getField @"unwrapWidget_legacy_t" x0
      )

instance ( ty ~ M.WidgetT
         ) => BG.HasField "unwrapWidget_legacy_t" (BG.Ptr Widget_legacy_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapWidget_legacy_t")

instance HasCField.HasCField Widget_legacy_t "unwrapWidget_legacy_t" where

  type CFieldType Widget_legacy_t "unwrapWidget_legacy_t" =
    M.WidgetT

  offset# = \_ -> \_ -> 0
