List {getList = [DeclData (WithStruct (Struct {structName = "ExampleStruct", structConstr = "MkExampleStruct", structFields = ("t1",HsType "StructFieldTypeTODO") ::: ("t2",HsType "StructFieldTypeTODO") ::: ("m1",HsType "StructFieldTypeTODO") ::: ("m2",HsType "StructFieldTypeTODO") ::: VNil}) (MkDataDecl)), DeclInstance (InstanceStorable (WithStruct (Struct {structName = "ExampleStruct", structConstr = "MkExampleStruct", structFields = ("t1",HsType "StructFieldTypeTODO") ::: ("t2",HsType "StructFieldTypeTODO") ::: ("m1",HsType "StructFieldTypeTODO") ::: ("m2",HsType "StructFieldTypeTODO") ::: VNil}) (StorableInstance {storableSizeOf = 16, storableAlignment = 4, storablePeek = Lambda (\x0 -> Ap (IntroStruct (Struct {structName = "ExampleStruct", structConstr = "MkExampleStruct", structFields = ("t1",HsType "StructFieldTypeTODO") ::: ("t2",HsType "StructFieldTypeTODO") ::: ("m1",HsType "StructFieldTypeTODO") ::: ("m2",HsType "StructFieldTypeTODO") ::: VNil})) [PeekByteOff x0 0, PeekByteOff x0 32, PeekByteOff x0 64, PeekByteOff x0 96]), storablePoke = Lambda (\x0 -> ElimStruct (Struct {structName = "ExampleStruct", structConstr = "MkExampleStruct", structFields = ("t1",HsType "StructFieldTypeTODO") ::: ("t2",HsType "StructFieldTypeTODO") ::: ("m1",HsType "StructFieldTypeTODO") ::: ("m2",HsType "StructFieldTypeTODO") ::: VNil}) (\(x1 ::: x2 ::: x3 ::: x4 ::: VNil) -> (Seq (List {getList = [PokeByteOff x0 0 x1, PokeByteOff x0 32 x2, PokeByteOff x0 64 x3, PokeByteOff x0 96 x4]}))))})))]}