[DeclNewtype (Newtype {newtypeName = "CM1", newtypeConstr = "MkCM1", newtypeField = "unCM1", newtypeType = HsPrimType HsPrimCInt})
,DeclNewtype (Newtype {newtypeName = "CM2", newtypeConstr = "MkCM2", newtypeField = "unCM2", newtypeType = HsPrimType HsPrimCChar})
,DeclNewtype (Newtype {newtypeName = "CT1", newtypeConstr = "MkCT1", newtypeField = "unCT1", newtypeType = HsPrimType HsPrimCInt})
,DeclNewtypeInstance Storable "CT1"
,DeclNewtype (Newtype {newtypeName = "CT2", newtypeConstr = "MkCT2", newtypeField = "unCT2", newtypeType = HsPrimType HsPrimCChar})
,DeclNewtypeInstance Storable "CT2"
,DeclData (WithStruct (Struct {structName = "CExampleStruct", structConstr = "MkCExampleStruct", structFields = ("cExampleStruct_t1",HsTypRef "CT1") ::: ("cExampleStruct_t2",HsTypRef "CT2") ::: ("cExampleStruct_m1",HsTypRef "CM1") ::: ("cExampleStruct_m2",HsTypRef "CM2") ::: VNil}) (MkDataDecl))
,DeclInstance (InstanceStorable (WithStruct (Struct {structName = "CExampleStruct", structConstr = "MkCExampleStruct", structFields = ("cExampleStruct_t1",HsTypRef "CT1") ::: ("cExampleStruct_t2",HsTypRef "CT2") ::: ("cExampleStruct_m1",HsTypRef "CM1") ::: ("cExampleStruct_m2",HsTypRef "CM2") ::: VNil}) (StorableInstance {storableSizeOf = 16, storableAlignment = 4, storablePeek = Lambda (\x0 -> Ap (IntroStruct (Struct {structName = "CExampleStruct", structConstr = "MkCExampleStruct", structFields = ("cExampleStruct_t1",HsTypRef "CT1") ::: ("cExampleStruct_t2",HsTypRef "CT2") ::: ("cExampleStruct_m1",HsTypRef "CM1") ::: ("cExampleStruct_m2",HsTypRef "CM2") ::: VNil})) [PeekByteOff x0 0, PeekByteOff x0 4, PeekByteOff x0 8, PeekByteOff x0 12]), storablePoke = Lambda (\x0 -> ElimStruct (Struct {structName = "CExampleStruct", structConstr = "MkCExampleStruct", structFields = ("cExampleStruct_t1",HsTypRef "CT1") ::: ("cExampleStruct_t2",HsTypRef "CT2") ::: ("cExampleStruct_m1",HsTypRef "CM1") ::: ("cExampleStruct_m2",HsTypRef "CM2") ::: VNil}) (\(x1 ::: x2 ::: x3 ::: x4 ::: VNil) -> (Seq [PokeByteOff x0 0 x1
,PokeByteOff x0 4 x2
,PokeByteOff x0 8 x3
,PokeByteOff x0 12 x4
])))})))
]
