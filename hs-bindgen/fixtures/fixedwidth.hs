DeclNewtype (Newtype {newtypeName = "Uint64_t", newtypeConstr = "Uint64_t", newtypeField = Field {fieldName = "unUint64_t", fieldType = HsPrimType HsPrimCULong, fieldOrigin = FieldOriginNone}, newtypeOrigin = NewtypeOriginTypedef (Typedef {typedefName = "uint64_t", typedefType = TypePrim (PrimIntegral (PrimLong Unsigned)), typedefSourceLoc = "musl-include/bits/alltypes.h:136:25"})})
DeclNewtypeInstance Storable "Uint64_t"
DeclNewtype (Newtype {newtypeName = "Uint32_t", newtypeConstr = "Uint32_t", newtypeField = Field {fieldName = "unUint32_t", fieldType = HsPrimType HsPrimCUInt, fieldOrigin = FieldOriginNone}, newtypeOrigin = NewtypeOriginTypedef (Typedef {typedefName = "uint32_t", typedefType = TypePrim (PrimIntegral (PrimInt Unsigned)), typedefSourceLoc = "musl-include/bits/alltypes.h:131:25"})})
DeclNewtypeInstance Storable "Uint32_t"
DeclData (Struct {structName = "Foo", structConstr = "Foo", structFields = Field {fieldName = "foo_sixty_four", fieldType = HsTypRef "Uint64_t", fieldOrigin = FieldOriginStructField (StructField {fieldName = "sixty_four", fieldOffset = 0, fieldType = TypeTypedef "uint64_t", fieldSourceLoc = "examples/fixedwidth.h:4:11"})} ::: Field {fieldName = "foo_thirty_two", fieldType = HsTypRef "Uint32_t", fieldOrigin = FieldOriginStructField (StructField {fieldName = "thirty_two", fieldOffset = 64, fieldType = TypeTypedef "uint32_t", fieldSourceLoc = "examples/fixedwidth.h:5:11"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (DeclNameTag "foo") DeclPathTop, structSizeof = 16, structAlignment = 8, structFields = [StructField {fieldName = "sixty_four", fieldOffset = 0, fieldType = TypeTypedef "uint64_t", fieldSourceLoc = "examples/fixedwidth.h:4:11"},StructField {fieldName = "thirty_two", fieldOffset = 64, fieldType = TypeTypedef "uint32_t", fieldSourceLoc = "examples/fixedwidth.h:5:11"}], structSourceLoc = "examples/fixedwidth.h:3:8"})})
DeclInstance (InstanceStorable (Struct {structName = "Foo", structConstr = "Foo", structFields = Field {fieldName = "foo_sixty_four", fieldType = HsTypRef "Uint64_t", fieldOrigin = FieldOriginStructField (StructField {fieldName = "sixty_four", fieldOffset = 0, fieldType = TypeTypedef "uint64_t", fieldSourceLoc = "examples/fixedwidth.h:4:11"})} ::: Field {fieldName = "foo_thirty_two", fieldType = HsTypRef "Uint32_t", fieldOrigin = FieldOriginStructField (StructField {fieldName = "thirty_two", fieldOffset = 64, fieldType = TypeTypedef "uint32_t", fieldSourceLoc = "examples/fixedwidth.h:5:11"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (DeclNameTag "foo") DeclPathTop, structSizeof = 16, structAlignment = 8, structFields = [StructField {fieldName = "sixty_four", fieldOffset = 0, fieldType = TypeTypedef "uint64_t", fieldSourceLoc = "examples/fixedwidth.h:4:11"},StructField {fieldName = "thirty_two", fieldOffset = 64, fieldType = TypeTypedef "uint32_t", fieldSourceLoc = "examples/fixedwidth.h:5:11"}], structSourceLoc = "examples/fixedwidth.h:3:8"})}) (StorableInstance {storableSizeOf = 16, storableAlignment = 8, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "Foo", structConstr = "Foo", structFields = Field {fieldName = "foo_sixty_four", fieldType = HsTypRef "Uint64_t", fieldOrigin = FieldOriginStructField (StructField {fieldName = "sixty_four", fieldOffset = 0, fieldType = TypeTypedef "uint64_t", fieldSourceLoc = "examples/fixedwidth.h:4:11"})} ::: Field {fieldName = "foo_thirty_two", fieldType = HsTypRef "Uint32_t", fieldOrigin = FieldOriginStructField (StructField {fieldName = "thirty_two", fieldOffset = 64, fieldType = TypeTypedef "uint32_t", fieldSourceLoc = "examples/fixedwidth.h:5:11"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (DeclNameTag "foo") DeclPathTop, structSizeof = 16, structAlignment = 8, structFields = [StructField {fieldName = "sixty_four", fieldOffset = 0, fieldType = TypeTypedef "uint64_t", fieldSourceLoc = "examples/fixedwidth.h:4:11"},StructField {fieldName = "thirty_two", fieldOffset = 64, fieldType = TypeTypedef "uint32_t", fieldSourceLoc = "examples/fixedwidth.h:5:11"}], structSourceLoc = "examples/fixedwidth.h:3:8"})})) [PeekByteOff 0 0,PeekByteOff 0 8]), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "Foo", structConstr = "Foo", structFields = Field {fieldName = "foo_sixty_four", fieldType = HsTypRef "Uint64_t", fieldOrigin = FieldOriginStructField (StructField {fieldName = "sixty_four", fieldOffset = 0, fieldType = TypeTypedef "uint64_t", fieldSourceLoc = "examples/fixedwidth.h:4:11"})} ::: Field {fieldName = "foo_thirty_two", fieldType = HsTypRef "Uint32_t", fieldOrigin = FieldOriginStructField (StructField {fieldName = "thirty_two", fieldOffset = 64, fieldType = TypeTypedef "uint32_t", fieldSourceLoc = "examples/fixedwidth.h:5:11"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (DeclNameTag "foo") DeclPathTop, structSizeof = 16, structAlignment = 8, structFields = [StructField {fieldName = "sixty_four", fieldOffset = 0, fieldType = TypeTypedef "uint64_t", fieldSourceLoc = "examples/fixedwidth.h:4:11"},StructField {fieldName = "thirty_two", fieldOffset = 64, fieldType = TypeTypedef "uint32_t", fieldSourceLoc = "examples/fixedwidth.h:5:11"}], structSourceLoc = "examples/fixedwidth.h:3:8"})}) 2 (Seq [PokeByteOff 3 0 0,PokeByteOff 3 8 1])))}))
