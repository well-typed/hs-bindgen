DeclNewtype (Newtype {newtypeName = "CFoo", newtypeConstr = "MkCFoo", newtypeField = "unCFoo", newtypeType = HsPrimType HsPrimCUInt})
DeclInstance (InstanceStorable (Struct {structName = "CFoo", structConstr = "MkCFoo", structFields = ("unCFoo",HsPrimType HsPrimCUInt) ::: VNil}) (StorableInstance {storableSizeOf = 4, storableAlignment = 4, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "CFoo", structConstr = "MkCFoo", structFields = ("unCFoo",HsPrimType HsPrimCUInt) ::: VNil})) [PeekByteOff 0 0]), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "CFoo", structConstr = "MkCFoo", structFields = ("unCFoo",HsPrimType HsPrimCUInt) ::: VNil}) 1 (Seq [PokeByteOff 2 0 0])))}))
DeclNewtype (Newtype {newtypeName = "CFoo", newtypeConstr = "MkCFoo", newtypeField = "unCFoo", newtypeType = HsPrimType HsPrimCDouble})
DeclNewtypeInstance Storable "CFoo"
