[DeclNewtype (Newtype {newtypeName = "CMyEnum", newtypeConstr = "MkCMyEnum", newtypeField = "unCMyEnum", newtypeType = HsPrimType HsPrimCUInt})
,DeclInstance (InstanceStorable (WithStruct (Struct {structName = "CMyEnum", structConstr = "MkCMyEnum", structFields = ("unCMyEnum",HsPrimType HsPrimCUInt) ::: VNil}) (StorableInstance {storableSizeOf = 4, storableAlignment = 4, storablePeek = Lambda (\x0 -> Ap (IntroStruct (Struct {structName = "CMyEnum", structConstr = "MkCMyEnum", structFields = ("unCMyEnum",HsPrimType HsPrimCUInt) ::: VNil})) [PeekByteOff x0 0]), storablePoke = Lambda (\x0 -> ElimStruct (Struct {structName = "CMyEnum", structConstr = "MkCMyEnum", structFields = ("unCMyEnum",HsPrimType HsPrimCUInt) ::: VNil}) (\(x1 ::: VNil) -> (Seq [PokeByteOff x0 0 x1
])))})))
]
