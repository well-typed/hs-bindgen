DeclNewtype (Newtype {newtypeName = "Foo", newtypeConstr = "Foo", newtypeField = Field {fieldName = "unFoo", fieldType = HsPrimType HsPrimCUInt}})
DeclInstance (InstanceStorable (Struct {structName = "Foo", structConstr = "Foo", structFields = Field {fieldName = "unFoo", fieldType = HsPrimType HsPrimCUInt} ::: VNil}) (StorableInstance {storableSizeOf = 4, storableAlignment = 4, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "Foo", structConstr = "Foo", structFields = Field {fieldName = "unFoo", fieldType = HsPrimType HsPrimCUInt} ::: VNil})) [PeekByteOff 0 0]), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "Foo", structConstr = "Foo", structFields = Field {fieldName = "unFoo", fieldType = HsPrimType HsPrimCUInt} ::: VNil}) 1 (Seq [PokeByteOff 2 0 0])))}))
DeclPatSyn (PatSyn {patSynName = "FOO1", patSynType = "Foo", patSynConstr = "Foo", patSynValue = 0})
DeclPatSyn (PatSyn {patSynName = "FOO2", patSynType = "Foo", patSynConstr = "Foo", patSynValue = 1})
DeclNewtype (Newtype {newtypeName = "Foo", newtypeConstr = "Foo", newtypeField = Field {fieldName = "unFoo", fieldType = HsPrimType HsPrimCDouble}})
DeclNewtypeInstance Storable "Foo"
