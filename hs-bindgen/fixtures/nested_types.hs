List {getList = [DeclData (WithStruct (Struct {structName = "CFoo", structConstr = "MkCFoo", structFields = ("cFoo_i",HsType "StructFieldTypeTODO") ::: ("cFoo_c",HsType "StructFieldTypeTODO") ::: VNil}) (MkDataDecl)), DeclInstance (InstanceStorable (WithStruct (Struct {structName = "CFoo", structConstr = "MkCFoo", structFields = ("cFoo_i",HsType "StructFieldTypeTODO") ::: ("cFoo_c",HsType "StructFieldTypeTODO") ::: VNil}) (StorableInstance {storableSizeOf = 8, storableAlignment = 4, storablePeek = Lambda (\x0 -> Ap (IntroStruct (Struct {structName = "CFoo", structConstr = "MkCFoo", structFields = ("cFoo_i",HsType "StructFieldTypeTODO") ::: ("cFoo_c",HsType "StructFieldTypeTODO") ::: VNil})) [PeekByteOff x0 0, PeekByteOff x0 32]), storablePoke = Lambda (\x0 -> ElimStruct (Struct {structName = "CFoo", structConstr = "MkCFoo", structFields = ("cFoo_i",HsType "StructFieldTypeTODO") ::: ("cFoo_c",HsType "StructFieldTypeTODO") ::: VNil}) (\(x1 ::: x2 ::: VNil) -> (Seq (List {getList = [PokeByteOff x0 0 x1, PokeByteOff x0 32 x2]}))))}))), DeclData (WithStruct (Struct {structName = "CBar", structConstr = "MkCBar", structFields = ("cBar_foo1",HsType "StructFieldTypeTODO") ::: ("cBar_foo2",HsType "StructFieldTypeTODO") ::: VNil}) (MkDataDecl)), DeclInstance (InstanceStorable (WithStruct (Struct {structName = "CBar", structConstr = "MkCBar", structFields = ("cBar_foo1",HsType "StructFieldTypeTODO") ::: ("cBar_foo2",HsType "StructFieldTypeTODO") ::: VNil}) (StorableInstance {storableSizeOf = 16, storableAlignment = 4, storablePeek = Lambda (\x0 -> Ap (IntroStruct (Struct {structName = "CBar", structConstr = "MkCBar", structFields = ("cBar_foo1",HsType "StructFieldTypeTODO") ::: ("cBar_foo2",HsType "StructFieldTypeTODO") ::: VNil})) [PeekByteOff x0 0, PeekByteOff x0 64]), storablePoke = Lambda (\x0 -> ElimStruct (Struct {structName = "CBar", structConstr = "MkCBar", structFields = ("cBar_foo1",HsType "StructFieldTypeTODO") ::: ("cBar_foo2",HsType "StructFieldTypeTODO") ::: VNil}) (\(x1 ::: x2 ::: VNil) -> (Seq (List {getList = [PokeByteOff x0 0 x1, PokeByteOff x0 64 x2]}))))})))]}