List {getList = [DeclInstance (InstanceStorable (WithStruct (Struct {structName = "First", structConstr = "MkFirst", structFields = "unfirst" ::: VNil}) (StorableInstance {storableSizeOf = 4, storableAlignment = 4, storablePeek = Lambda (\x0 -> Ap (IntroStruct (Struct {structName = "First", structConstr = "MkFirst", structFields = "unfirst" ::: VNil})) [PeekByteOff x0 0]), storablePoke = Lambda (\x0 -> ElimStruct (Struct {structName = "First", structConstr = "MkFirst", structFields = "unfirst" ::: VNil}) (\(x1 ::: VNil) -> (Seq (List {getList = [PokeByteOff x0 0 x1]}))))}))), DeclInstance (InstanceStorable (WithStruct (Struct {structName = "Second", structConstr = "MkSecond", structFields = "unsecond" ::: VNil}) (StorableInstance {storableSizeOf = 4, storableAlignment = 4, storablePeek = Lambda (\x0 -> Ap (IntroStruct (Struct {structName = "Second", structConstr = "MkSecond", structFields = "unsecond" ::: VNil})) [PeekByteOff x0 0]), storablePoke = Lambda (\x0 -> ElimStruct (Struct {structName = "Second", structConstr = "MkSecond", structFields = "unsecond" ::: VNil}) (\(x1 ::: VNil) -> (Seq (List {getList = [PokeByteOff x0 0 x1]}))))}))), DeclInstance (InstanceStorable (WithStruct (Struct {structName = "Same", structConstr = "MkSame", structFields = "unsame" ::: VNil}) (StorableInstance {storableSizeOf = 4, storableAlignment = 4, storablePeek = Lambda (\x0 -> Ap (IntroStruct (Struct {structName = "Same", structConstr = "MkSame", structFields = "unsame" ::: VNil})) [PeekByteOff x0 0]), storablePoke = Lambda (\x0 -> ElimStruct (Struct {structName = "Same", structConstr = "MkSame", structFields = "unsame" ::: VNil}) (\(x1 ::: VNil) -> (Seq (List {getList = [PokeByteOff x0 0 x1]}))))}))), DeclInstance (InstanceStorable (WithStruct (Struct {structName = "Packad", structConstr = "MkPackad", structFields = "unpackad" ::: VNil}) (StorableInstance {storableSizeOf = 1, storableAlignment = 1, storablePeek = Lambda (\x0 -> Ap (IntroStruct (Struct {structName = "Packad", structConstr = "MkPackad", structFields = "unpackad" ::: VNil})) [PeekByteOff x0 0]), storablePoke = Lambda (\x0 -> ElimStruct (Struct {structName = "Packad", structConstr = "MkPackad", structFields = "unpackad" ::: VNil}) (\(x1 ::: VNil) -> (Seq (List {getList = [PokeByteOff x0 0 x1]}))))})))]}