-- | Utilities for dealing with fields uniformly
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
import HsBindgen.IR.Pass.Types (PassTypes (Types))

flattenFields :: [C.Field Final] -> [Field]
flattenFields = concatMap flattenField

flattenField :: C.Field Final -> [Field]
flattenField = \case
    C.FieldRegular field  -> [RegularField field]
    C.FieldImplicit field -> ImplicitField field : fmap (IndirectField field) field.indirect

data Field =
    RegularField  (C.RegularField Final)
  | ImplicitField (C.ImplicitField Final)
  | IndirectField (C.ImplicitField Final) (C.IndirectField Final)

getFieldInfo :: Field -> C.FieldInfo Final
getFieldInfo = \case
    RegularField  field -> field.info
    ImplicitField field -> field.info
    IndirectField _impField indField -> indField.info

getFieldTyp :: Field -> Types Final
getFieldTyp = \case
    RegularField  field -> field.typ
    ImplicitField field -> field.typ
    IndirectField _impField indField -> indField.typ

getFieldWidth :: Field -> Maybe Int
getFieldWidth = \case
    RegularField  field -> field.width
    ImplicitField field -> field.width
    IndirectField _impField indField -> indField.width

getFieldOffset :: Field -> Int
getFieldOffset = \case
    RegularField  field -> field.offset
    ImplicitField field -> field.offset
    IndirectField _impField indField -> indField.offset
