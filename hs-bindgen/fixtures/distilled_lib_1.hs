DeclVar (VarDecl {varDeclName = "a", varDeclType = ForallTy {forallTySize = 1, forallTyBinders = "a" ::: VNil, forallTy = QuantTy {quantTyCts = [ClassTy Integral (TyVarTy 0 ::: VNil)], quantTyBody = TyVarTy 0}}, varDeclBody = VarDeclIntegral 5 HsPrimCInt})
DeclVar (VarDecl {varDeclName = "b", varDeclType = ForallTy {forallTySize = 1, forallTyBinders = "a" ::: VNil, forallTy = QuantTy {quantTyCts = [ClassTy Integral (TyVarTy 0 ::: VNil)], quantTyBody = TyVarTy 0}}, varDeclBody = VarDeclIntegral 3 HsPrimCInt})
DeclVar (VarDecl {varDeclName = "sOME_DEFINED_CONSTANT", varDeclType = ForallTy {forallTySize = 1, forallTyBinders = "a" ::: VNil, forallTy = QuantTy {quantTyCts = [ClassTy Integral (TyVarTy 0 ::: VNil)], quantTyBody = TyVarTy 0}}, varDeclBody = VarDeclIntegral 4 HsPrimCInt})
DeclVar (VarDecl {varDeclName = "a_DEFINE_0", varDeclType = ForallTy {forallTySize = 1, forallTyBinders = "a" ::: VNil, forallTy = QuantTy {quantTyCts = [ClassTy Integral (TyVarTy 0 ::: VNil)], quantTyBody = TyVarTy 0}}, varDeclBody = VarDeclIntegral 0 HsPrimCInt})
DeclVar (VarDecl {varDeclName = "a_DEFINE_1", varDeclType = ForallTy {forallTySize = 0, forallTyBinders = VNil, forallTy = QuantTy {quantTyCts = [], quantTyBody = TyConAppTy (TyConApp UInt VNil)}}, varDeclBody = VarDeclIntegral 20560 HsPrimCUInt})
DeclVar (VarDecl {varDeclName = "a_DEFINE_2", varDeclType = ForallTy {forallTySize = 1, forallTyBinders = "a" ::: VNil, forallTy = QuantTy {quantTyCts = [ClassTy Integral (TyVarTy 0 ::: VNil)], quantTyBody = TyVarTy 0}}, varDeclBody = VarDeclIntegral 2 HsPrimCInt})
DeclVar (VarDecl {varDeclName = "tWO_ARGS", varDeclType = ForallTy {forallTySize = 1, forallTyBinders = "a" ::: VNil, forallTy = QuantTy {quantTyCts = [ClassTy Integral (TyVarTy 0 ::: VNil)], quantTyBody = TyVarTy 0}}, varDeclBody = VarDeclIntegral 13398 HsPrimCInt})
DeclData (Struct {structName = "CX", structConstr = "MkCX", structFields = ("cX_foo",HsPrimType HsPrimCInt) ::: ("cX_bar",HsPrimType HsPrimCChar) ::: VNil})
DeclInstance (InstanceStorable (Struct {structName = "CX", structConstr = "MkCX", structFields = ("cX_foo",HsPrimType HsPrimCInt) ::: ("cX_bar",HsPrimType HsPrimCChar) ::: VNil}) (StorableInstance {storableSizeOf = 8, storableAlignment = 4, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "CX", structConstr = "MkCX", structFields = ("cX_foo",HsPrimType HsPrimCInt) ::: ("cX_bar",HsPrimType HsPrimCChar) ::: VNil})) [PeekByteOff 0 0,PeekByteOff 0 4]), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "CX", structConstr = "MkCX", structFields = ("cX_foo",HsPrimType HsPrimCInt) ::: ("cX_bar",HsPrimType HsPrimCChar) ::: VNil}) 2 (Seq [PokeByteOff 3 0 0,PokeByteOff 3 4 1])))}))
DeclNewtype (Newtype {newtypeName = "CAnotherTypedefStructT", newtypeConstr = "MkCAnotherTypedefStructT", newtypeField = "unCAnotherTypedefStructT", newtypeType = HsTypRef "CStruct'0020anotherTypedefStructT"})
DeclNewtypeInstance Storable "CAnotherTypedefStructT"
DeclNewtype (Newtype {newtypeName = "CX", newtypeConstr = "MkCX", newtypeField = "unCX", newtypeType = HsPrimType HsPrimCUInt})
DeclInstance (InstanceStorable (Struct {structName = "CX", structConstr = "MkCX", structFields = ("unCX",HsPrimType HsPrimCUInt) ::: VNil}) (StorableInstance {storableSizeOf = 4, storableAlignment = 4, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "CX", structConstr = "MkCX", structFields = ("unCX",HsPrimType HsPrimCUInt) ::: VNil})) [PeekByteOff 0 0]), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "CX", structConstr = "MkCX", structFields = ("unCX",HsPrimType HsPrimCUInt) ::: VNil}) 1 (Seq [PokeByteOff 2 0 0])))}))
DeclNewtype (Newtype {newtypeName = "CAnotherTypedefEnumE", newtypeConstr = "MkCAnotherTypedefEnumE", newtypeField = "unCAnotherTypedefEnumE", newtypeType = HsTypRef "CEnum'0020anotherTypedefEnumE"})
DeclNewtypeInstance Storable "CAnotherTypedefEnumE"
DeclNewtype (Newtype {newtypeName = "CATypeT", newtypeConstr = "MkCATypeT", newtypeField = "unCATypeT", newtypeType = HsPrimType HsPrimCInt})
DeclNewtypeInstance Storable "CATypeT"
DeclNewtype (Newtype {newtypeName = "CVarT", newtypeConstr = "MkCVarT", newtypeField = "unCVarT", newtypeType = HsPrimType HsPrimCInt})
DeclNewtypeInstance Storable "CVarT"
DeclData (Struct {structName = "CATypedefStruct", structConstr = "MkCATypedefStruct", structFields = ("cATypedefStruct_field_0",HsTypRef "CBool'") ::: ("cATypedefStruct_field_1",HsTypRef "CUint8T") ::: ("cATypedefStruct_field_2",HsTypRef "CUint16T") ::: ("cATypedefStruct_field_3",HsTypRef "CUint32T") ::: ("cATypedefStruct_field_4",HsTypRef "CAnotherTypedefStructT") ::: ("cATypedefStruct_field_5",HsPtr (HsTypRef "CAnotherTypedefStructT")) ::: ("cATypedefStruct_field_6",HsPtr (HsPrimType HsPrimVoid)) ::: ("cATypedefStruct_field_7",HsConstArray 7 (HsTypRef "CUint32T")) ::: ("cATypedefStruct_field_8",HsTypRef "CAnotherTypedefEnumE") ::: ("cATypedefStruct_field_9",HsTypRef "CAnotherTypedefEnumE") ::: ("cATypedefStruct_field_10",HsTypRef "CAnotherTypedefEnumE") ::: VNil})
DeclInstance (InstanceStorable (Struct {structName = "CATypedefStruct", structConstr = "MkCATypedefStruct", structFields = ("cATypedefStruct_field_0",HsTypRef "CBool'") ::: ("cATypedefStruct_field_1",HsTypRef "CUint8T") ::: ("cATypedefStruct_field_2",HsTypRef "CUint16T") ::: ("cATypedefStruct_field_3",HsTypRef "CUint32T") ::: ("cATypedefStruct_field_4",HsTypRef "CAnotherTypedefStructT") ::: ("cATypedefStruct_field_5",HsPtr (HsTypRef "CAnotherTypedefStructT")) ::: ("cATypedefStruct_field_6",HsPtr (HsPrimType HsPrimVoid)) ::: ("cATypedefStruct_field_7",HsConstArray 7 (HsTypRef "CUint32T")) ::: ("cATypedefStruct_field_8",HsTypRef "CAnotherTypedefEnumE") ::: ("cATypedefStruct_field_9",HsTypRef "CAnotherTypedefEnumE") ::: ("cATypedefStruct_field_10",HsTypRef "CAnotherTypedefEnumE") ::: VNil}) (StorableInstance {storableSizeOf = 140, storableAlignment = 1, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "CATypedefStruct", structConstr = "MkCATypedefStruct", structFields = ("cATypedefStruct_field_0",HsTypRef "CBool'") ::: ("cATypedefStruct_field_1",HsTypRef "CUint8T") ::: ("cATypedefStruct_field_2",HsTypRef "CUint16T") ::: ("cATypedefStruct_field_3",HsTypRef "CUint32T") ::: ("cATypedefStruct_field_4",HsTypRef "CAnotherTypedefStructT") ::: ("cATypedefStruct_field_5",HsPtr (HsTypRef "CAnotherTypedefStructT")) ::: ("cATypedefStruct_field_6",HsPtr (HsPrimType HsPrimVoid)) ::: ("cATypedefStruct_field_7",HsConstArray 7 (HsTypRef "CUint32T")) ::: ("cATypedefStruct_field_8",HsTypRef "CAnotherTypedefEnumE") ::: ("cATypedefStruct_field_9",HsTypRef "CAnotherTypedefEnumE") ::: ("cATypedefStruct_field_10",HsTypRef "CAnotherTypedefEnumE") ::: VNil})) [PeekByteOff 0 0,PeekByteOff 0 1,PeekByteOff 0 2,PeekByteOff 0 4,PeekByteOff 0 8,PeekByteOff 0 16,PeekByteOff 0 24,PeekByteOff 0 32,PeekByteOff 0 60,PeekByteOff 0 64,PeekByteOff 0 80]), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "CATypedefStruct", structConstr = "MkCATypedefStruct", structFields = ("cATypedefStruct_field_0",HsTypRef "CBool'") ::: ("cATypedefStruct_field_1",HsTypRef "CUint8T") ::: ("cATypedefStruct_field_2",HsTypRef "CUint16T") ::: ("cATypedefStruct_field_3",HsTypRef "CUint32T") ::: ("cATypedefStruct_field_4",HsTypRef "CAnotherTypedefStructT") ::: ("cATypedefStruct_field_5",HsPtr (HsTypRef "CAnotherTypedefStructT")) ::: ("cATypedefStruct_field_6",HsPtr (HsPrimType HsPrimVoid)) ::: ("cATypedefStruct_field_7",HsConstArray 7 (HsTypRef "CUint32T")) ::: ("cATypedefStruct_field_8",HsTypRef "CAnotherTypedefEnumE") ::: ("cATypedefStruct_field_9",HsTypRef "CAnotherTypedefEnumE") ::: ("cATypedefStruct_field_10",HsTypRef "CAnotherTypedefEnumE") ::: VNil}) 11 (Seq [PokeByteOff 12 0 0,PokeByteOff 12 1 1,PokeByteOff 12 2 2,PokeByteOff 12 4 3,PokeByteOff 12 8 4,PokeByteOff 12 16 5,PokeByteOff 12 24 6,PokeByteOff 12 32 7,PokeByteOff 12 60 8,PokeByteOff 12 64 9,PokeByteOff 12 80 10])))}))
DeclNewtype (Newtype {newtypeName = "CATypedefStructT", newtypeConstr = "MkCATypedefStructT", newtypeField = "unCATypedefStructT", newtypeType = HsTypRef "CStruct'0020aTypedefStruct"})
DeclNewtypeInstance Storable "CATypedefStructT"
DeclNewtype (Newtype {newtypeName = "CX", newtypeConstr = "MkCX", newtypeField = "unCX", newtypeType = HsPrimType HsPrimCSChar})
DeclInstance (InstanceStorable (Struct {structName = "CX", structConstr = "MkCX", structFields = ("unCX",HsPrimType HsPrimCSChar) ::: VNil}) (StorableInstance {storableSizeOf = 1, storableAlignment = 1, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "CX", structConstr = "MkCX", structFields = ("unCX",HsPrimType HsPrimCSChar) ::: VNil})) [PeekByteOff 0 0]), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "CX", structConstr = "MkCX", structFields = ("unCX",HsPrimType HsPrimCSChar) ::: VNil}) 1 (Seq [PokeByteOff 2 0 0])))}))
DeclNewtype (Newtype {newtypeName = "CATypedefEnumE", newtypeConstr = "MkCATypedefEnumE", newtypeField = "unCATypedefEnumE", newtypeType = HsTypRef "CEnum'0020aTypedefEnumE"})
DeclNewtypeInstance Storable "CATypedefEnumE"
DeclNewtype (Newtype {newtypeName = "CCallbackT", newtypeConstr = "MkCCallbackT", newtypeField = "unCCallbackT", newtypeType = HsPtr (HsPrimType HsPrimVoid)})
DeclNewtypeInstance Storable "CCallbackT"