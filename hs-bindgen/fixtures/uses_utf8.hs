DeclNewtype (Newtype {newtypeName = "CMyEnum", newtypeConstr = "MkCMyEnum", newtypeField = "unCMyEnum", newtypeType = HsPrimType HsPrimCUInt})
DeclInstance (InstanceStorable (Struct {structName = "CMyEnum", structConstr = "MkCMyEnum", structFields = ("unCMyEnum",HsPrimType HsPrimCUInt) ::: VNil}) (StorableInstance {storableSizeOf = 4, storableAlignment = 4, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "CMyEnum", structConstr = "MkCMyEnum", structFields = ("unCMyEnum",HsPrimType HsPrimCUInt) ::: VNil})) [PeekByteOff 0 0]), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "CMyEnum", structConstr = "MkCMyEnum", structFields = ("unCMyEnum",HsPrimType HsPrimCUInt) ::: VNil}) 1 (Seq [PokeByteOff 2 0 0])))}))
