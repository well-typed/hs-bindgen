DeclData (Struct {structName = "CBools1", structConstr = "MkCBools1", structFields = ("cBools1_x",HsPrimType HsPrimVoid) ::: ("cBools1_y",HsPrimType HsPrimVoid) ::: VNil})
DeclInstance (InstanceStorable (Struct {structName = "CBools1", structConstr = "MkCBools1", structFields = ("cBools1_x",HsPrimType HsPrimVoid) ::: ("cBools1_y",HsPrimType HsPrimVoid) ::: VNil}) (StorableInstance {storableSizeOf = 2, storableAlignment = 1, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "CBools1", structConstr = "MkCBools1", structFields = ("cBools1_x",HsPrimType HsPrimVoid) ::: ("cBools1_y",HsPrimType HsPrimVoid) ::: VNil})) [PeekByteOff 0 0,PeekByteOff 0 1]), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "CBools1", structConstr = "MkCBools1", structFields = ("cBools1_x",HsPrimType HsPrimVoid) ::: ("cBools1_y",HsPrimType HsPrimVoid) ::: VNil}) 2 (Seq [PokeByteOff 3 0 0,PokeByteOff 3 1 1])))}))
DeclData (Struct {structName = "CBools2", structConstr = "MkCBools2", structFields = ("cBools2_x",HsTypRef "CBool'") ::: ("cBools2_y",HsTypRef "CBool'") ::: VNil})
DeclInstance (InstanceStorable (Struct {structName = "CBools2", structConstr = "MkCBools2", structFields = ("cBools2_x",HsTypRef "CBool'") ::: ("cBools2_y",HsTypRef "CBool'") ::: VNil}) (StorableInstance {storableSizeOf = 2, storableAlignment = 1, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "CBools2", structConstr = "MkCBools2", structFields = ("cBools2_x",HsTypRef "CBool'") ::: ("cBools2_y",HsTypRef "CBool'") ::: VNil})) [PeekByteOff 0 0,PeekByteOff 0 1]), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "CBools2", structConstr = "MkCBools2", structFields = ("cBools2_x",HsTypRef "CBool'") ::: ("cBools2_y",HsTypRef "CBool'") ::: VNil}) 2 (Seq [PokeByteOff 3 0 0,PokeByteOff 3 1 1])))}))