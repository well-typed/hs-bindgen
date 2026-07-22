-- | Utilities for dealing with fields uniformly
--
-- TODO <https://github.com/well-typed/hs-bindgen/issues/2061>: add indirect
-- fields
module HsBindgen.Backend.Hs.Translation.Field (
    flattenFields
  , flattenField
  , Field(..)
  , getFieldInfo
  , getFieldTyp
  , getFieldWidth
  , getFieldOffset
  ) where

import HsBindgen.Frontend.Pass.Final
import HsBindgen.IR.C qualified as C

flattenFields :: [C.Field Final] -> [Field]
flattenFields = concatMap flattenField

flattenField :: C.Field Final -> [Field]
flattenField = \case
    C.FieldExplicit field -> [ExplicitField field]
    C.FieldImplicit field -> [ImplicitField field]

data Field =
    ExplicitField (C.ExplicitField Final)
  | ImplicitField (C.ImplicitField Final)

getFieldInfo :: Field -> C.FieldInfo Final
getFieldInfo = \case
    ExplicitField field -> field.info
    ImplicitField field -> field.info

getFieldTyp :: Field -> C.Type Final
getFieldTyp = \case
    ExplicitField field -> field.typ
    ImplicitField field -> field.typ

getFieldWidth :: Field -> Maybe Int
getFieldWidth = \case
    ExplicitField field -> field.width
    ImplicitField field -> field.width

getFieldOffset :: Field -> Int
getFieldOffset = \case
    ExplicitField field -> field.offset
    ImplicitField field -> field.offset
