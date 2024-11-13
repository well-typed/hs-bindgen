[DeclData (WithStruct (Struct {structName = "CS1", structConstr = "MkCS1", structFields = ("cS1_a",HsPrimType HsPrimCInt) ::: ("cS1_b",HsPrimType HsPrimCChar) ::: VNil}) (MkDataDecl))
,DeclInstance (InstanceStorable (WithStruct (Struct {structName = "CS1", structConstr = "MkCS1", structFields = ("cS1_a",HsPrimType HsPrimCInt) ::: ("cS1_b",HsPrimType HsPrimCChar) ::: VNil}) (StorableInstance {storableSizeOf = 8, storableAlignment = 4, storablePeek = Lambda (\x0 -> Ap (IntroStruct (Struct {structName = "CS1", structConstr = "MkCS1", structFields = ("cS1_a",HsPrimType HsPrimCInt) ::: ("cS1_b",HsPrimType HsPrimCChar) ::: VNil})) [PeekByteOff x0 0, PeekByteOff x0 4]), storablePoke = Lambda (\x0 -> ElimStruct (Struct {structName = "CS1", structConstr = "MkCS1", structFields = ("cS1_a",HsPrimType HsPrimCInt) ::: ("cS1_b",HsPrimType HsPrimCChar) ::: VNil}) (\(x1 ::: x2 ::: VNil) -> (Seq [PokeByteOff x0 0 x1
,PokeByteOff x0 4 x2
])))})))
,DeclData (WithStruct (Struct {structName = "CS2", structConstr = "MkCS2", structFields = ("cS2_a",HsPrimType HsPrimCChar) ::: ("cS2_b",HsPrimType HsPrimCInt) ::: ("cS2_c",HsPrimType HsPrimCFloat) ::: VNil}) (MkDataDecl))
,DeclInstance (InstanceStorable (WithStruct (Struct {structName = "CS2", structConstr = "MkCS2", structFields = ("cS2_a",HsPrimType HsPrimCChar) ::: ("cS2_b",HsPrimType HsPrimCInt) ::: ("cS2_c",HsPrimType HsPrimCFloat) ::: VNil}) (StorableInstance {storableSizeOf = 12, storableAlignment = 4, storablePeek = Lambda (\x0 -> Ap (IntroStruct (Struct {structName = "CS2", structConstr = "MkCS2", structFields = ("cS2_a",HsPrimType HsPrimCChar) ::: ("cS2_b",HsPrimType HsPrimCInt) ::: ("cS2_c",HsPrimType HsPrimCFloat) ::: VNil})) [PeekByteOff x0 0, PeekByteOff x0 4, PeekByteOff x0 8]), storablePoke = Lambda (\x0 -> ElimStruct (Struct {structName = "CS2", structConstr = "MkCS2", structFields = ("cS2_a",HsPrimType HsPrimCChar) ::: ("cS2_b",HsPrimType HsPrimCInt) ::: ("cS2_c",HsPrimType HsPrimCFloat) ::: VNil}) (\(x1 ::: x2 ::: x3 ::: VNil) -> (Seq [PokeByteOff x0 0 x1
,PokeByteOff x0 4 x2
,PokeByteOff x0 8 x3
])))})))
,DeclNewtype (Newtype {newtypeName = "CS2T", newtypeConstr = "MkCS2T", newtypeField = "unCS2T", newtypeType = HsTypRef "CStruct'0020S2"})
,DeclNewtypeInstance Storable "CS2T"
,DeclData (WithStruct (Struct {structName = "CX", structConstr = "MkCX", structFields = ("cX_a",HsPrimType HsPrimCChar) ::: VNil}) (MkDataDecl))
,DeclInstance (InstanceStorable (WithStruct (Struct {structName = "CX", structConstr = "MkCX", structFields = ("cX_a",HsPrimType HsPrimCChar) ::: VNil}) (StorableInstance {storableSizeOf = 1, storableAlignment = 1, storablePeek = Lambda (\x0 -> Ap (IntroStruct (Struct {structName = "CX", structConstr = "MkCX", structFields = ("cX_a",HsPrimType HsPrimCChar) ::: VNil})) [PeekByteOff x0 0]), storablePoke = Lambda (\x0 -> ElimStruct (Struct {structName = "CX", structConstr = "MkCX", structFields = ("cX_a",HsPrimType HsPrimCChar) ::: VNil}) (\(x1 ::: VNil) -> (Seq [PokeByteOff x0 0 x1
])))})))
,DeclNewtype (Newtype {newtypeName = "CS3T", newtypeConstr = "MkCS3T", newtypeField = "unCS3T", newtypeType = HsTypRef "CStruct'0020S3T"})
,DeclNewtypeInstance Storable "CS3T"
,DeclData (WithStruct (Struct {structName = "CS4", structConstr = "MkCS4", structFields = ("cS4_b",HsPrimType HsPrimCChar) ::: ("cS4_a",HsPrimType HsPrimCInt) ::: ("cS4_c",HsPtr (HsPrimType HsPrimCInt)) ::: VNil}) (MkDataDecl))
,DeclInstance (InstanceStorable (WithStruct (Struct {structName = "CS4", structConstr = "MkCS4", structFields = ("cS4_b",HsPrimType HsPrimCChar) ::: ("cS4_a",HsPrimType HsPrimCInt) ::: ("cS4_c",HsPtr (HsPrimType HsPrimCInt)) ::: VNil}) (StorableInstance {storableSizeOf = 16, storableAlignment = 8, storablePeek = Lambda (\x0 -> Ap (IntroStruct (Struct {structName = "CS4", structConstr = "MkCS4", structFields = ("cS4_b",HsPrimType HsPrimCChar) ::: ("cS4_a",HsPrimType HsPrimCInt) ::: ("cS4_c",HsPtr (HsPrimType HsPrimCInt)) ::: VNil})) [PeekByteOff x0 0, PeekByteOff x0 4, PeekByteOff x0 8]), storablePoke = Lambda (\x0 -> ElimStruct (Struct {structName = "CS4", structConstr = "MkCS4", structFields = ("cS4_b",HsPrimType HsPrimCChar) ::: ("cS4_a",HsPrimType HsPrimCInt) ::: ("cS4_c",HsPtr (HsPrimType HsPrimCInt)) ::: VNil}) (\(x1 ::: x2 ::: x3 ::: VNil) -> (Seq [PokeByteOff x0 0 x1
,PokeByteOff x0 4 x2
,PokeByteOff x0 8 x3
])))})))
]
