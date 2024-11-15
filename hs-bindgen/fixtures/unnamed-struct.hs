[DeclData (WithStruct (Struct {structName = "CX", structConstr = "MkCX", structFields = ("cX_foo",HsPrimType HsPrimCInt) ::: ("cX_bar",HsPrimType HsPrimCInt) ::: VNil}) (MkDataDecl))
,DeclInstance (InstanceStorable (WithStruct (Struct {structName = "CX", structConstr = "MkCX", structFields = ("cX_foo",HsPrimType HsPrimCInt) ::: ("cX_bar",HsPrimType HsPrimCInt) ::: VNil}) (StorableInstance {storableSizeOf = 8, storableAlignment = 4, storablePeek = Lambda (\x0 -> Ap (IntroStruct (Struct {structName = "CX", structConstr = "MkCX", structFields = ("cX_foo",HsPrimType HsPrimCInt) ::: ("cX_bar",HsPrimType HsPrimCInt) ::: VNil})) [PeekByteOff x0 0, PeekByteOff x0 4]), storablePoke = Lambda (\x0 -> ElimStruct (Struct {structName = "CX", structConstr = "MkCX", structFields = ("cX_foo",HsPrimType HsPrimCInt) ::: ("cX_bar",HsPrimType HsPrimCInt) ::: VNil}) (\(x1 ::: x2 ::: VNil) -> (Seq [PokeByteOff x0 0 x1
,PokeByteOff x0 4 x2
])))})))
]
