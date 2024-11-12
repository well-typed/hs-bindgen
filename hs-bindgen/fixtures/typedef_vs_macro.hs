DeclNewtype (Newtype {newtypeName = "CM1", newtypeConstr = "MkCM1", newtypeField = "unCM1", newtypeType = HsPrimType HsPrimCInt})
DeclNewtype (Newtype {newtypeName = "CM2", newtypeConstr = "MkCM2", newtypeField = "unCM2", newtypeType = HsPrimType HsPrimCChar})
DeclNewtype (Newtype {newtypeName = "CT1", newtypeConstr = "MkCT1", newtypeField = "unCT1", newtypeType = HsPrimType HsPrimCInt})
DeclNewtypeInstance Storable "CT1"
DeclNewtype (Newtype {newtypeName = "CT2", newtypeConstr = "MkCT2", newtypeField = "unCT2", newtypeType = HsPrimType HsPrimCChar})
DeclNewtypeInstance Storable "CT2"
DeclData (Struct {structName = "CExampleStruct", structConstr = "MkCExampleStruct", structFields = ("cExampleStruct_t1",HsTypRef "CT1") ::: ("cExampleStruct_t2",HsTypRef "CT2") ::: ("cExampleStruct_m1",HsTypRef "CM1") ::: ("cExampleStruct_m2",HsTypRef "CM2") ::: VNil})
DeclInstance (InstanceStorable (Struct {structName = "CExampleStruct", structConstr = "MkCExampleStruct", structFields = ("cExampleStruct_t1",HsTypRef "CT1") ::: ("cExampleStruct_t2",HsTypRef "CT2") ::: ("cExampleStruct_m1",HsTypRef "CM1") ::: ("cExampleStruct_m2",HsTypRef "CM2") ::: VNil}) (StorableInstance {storableSizeOf = 16, storableAlignment = 4, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "CExampleStruct", structConstr = "MkCExampleStruct", structFields = ("cExampleStruct_t1",HsTypRef "CT1") ::: ("cExampleStruct_t2",HsTypRef "CT2") ::: ("cExampleStruct_m1",HsTypRef "CM1") ::: ("cExampleStruct_m2",HsTypRef "CM2") ::: VNil})) [PeekByteOff 0 0,PeekByteOff 0 4,PeekByteOff 0 8,PeekByteOff 0 12]), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "CExampleStruct", structConstr = "MkCExampleStruct", structFields = ("cExampleStruct_t1",HsTypRef "CT1") ::: ("cExampleStruct_t2",HsTypRef "CT2") ::: ("cExampleStruct_m1",HsTypRef "CM1") ::: ("cExampleStruct_m2",HsTypRef "CM2") ::: VNil}) 4 (Seq [PokeByteOff 5 0 0,PokeByteOff 5 4 1,PokeByteOff 5 8 2,PokeByteOff 5 12 3])))}))
