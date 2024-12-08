DeclNewtype (Newtype {newtypeName = "M1", newtypeConstr = "M1", newtypeField = Field {fieldName = "unM1", fieldType = HsPrimType HsPrimCInt}})
DeclNewtype (Newtype {newtypeName = "M2", newtypeConstr = "M2", newtypeField = Field {fieldName = "unM2", fieldType = HsPrimType HsPrimCChar}})
DeclNewtype (Newtype {newtypeName = "T1", newtypeConstr = "T1", newtypeField = Field {fieldName = "unT1", fieldType = HsPrimType HsPrimCInt}})
DeclNewtypeInstance Storable "T1"
DeclNewtype (Newtype {newtypeName = "T2", newtypeConstr = "T2", newtypeField = Field {fieldName = "unT2", fieldType = HsPrimType HsPrimCChar}})
DeclNewtypeInstance Storable "T2"
DeclData (Struct {structName = "ExampleStruct", structConstr = "ExampleStruct", structFields = Field {fieldName = "exampleStruct_t1", fieldType = HsTypRef "T1"} ::: Field {fieldName = "exampleStruct_t2", fieldType = HsTypRef "T2"} ::: Field {fieldName = "exampleStruct_m1", fieldType = HsTypRef "M1"} ::: Field {fieldName = "exampleStruct_m2", fieldType = HsTypRef "M2"} ::: VNil})
DeclInstance (InstanceStorable (Struct {structName = "ExampleStruct", structConstr = "ExampleStruct", structFields = Field {fieldName = "exampleStruct_t1", fieldType = HsTypRef "T1"} ::: Field {fieldName = "exampleStruct_t2", fieldType = HsTypRef "T2"} ::: Field {fieldName = "exampleStruct_m1", fieldType = HsTypRef "M1"} ::: Field {fieldName = "exampleStruct_m2", fieldType = HsTypRef "M2"} ::: VNil}) (StorableInstance {storableSizeOf = 16, storableAlignment = 4, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "ExampleStruct", structConstr = "ExampleStruct", structFields = Field {fieldName = "exampleStruct_t1", fieldType = HsTypRef "T1"} ::: Field {fieldName = "exampleStruct_t2", fieldType = HsTypRef "T2"} ::: Field {fieldName = "exampleStruct_m1", fieldType = HsTypRef "M1"} ::: Field {fieldName = "exampleStruct_m2", fieldType = HsTypRef "M2"} ::: VNil})) [PeekByteOff 0 0,PeekByteOff 0 4,PeekByteOff 0 8,PeekByteOff 0 12]), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "ExampleStruct", structConstr = "ExampleStruct", structFields = Field {fieldName = "exampleStruct_t1", fieldType = HsTypRef "T1"} ::: Field {fieldName = "exampleStruct_t2", fieldType = HsTypRef "T2"} ::: Field {fieldName = "exampleStruct_m1", fieldType = HsTypRef "M1"} ::: Field {fieldName = "exampleStruct_m2", fieldType = HsTypRef "M2"} ::: VNil}) 4 (Seq [PokeByteOff 5 0 0,PokeByteOff 5 4 1,PokeByteOff 5 8 2,PokeByteOff 5 12 3])))}))
