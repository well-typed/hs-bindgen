DeclData (Struct {structName = "Foo", structConstr = "Foo", structFields = Field {fieldName = "foo_i", fieldType = HsPrimType HsPrimCInt, fieldOrigin = FieldOriginStructField (StructField {fieldName = "i", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/nested_types.h:2:9"})} ::: Field {fieldName = "foo_c", fieldType = HsPrimType HsPrimCChar, fieldOrigin = FieldOriginStructField (StructField {fieldName = "c", fieldOffset = 32, fieldType = TypePrim (PrimChar Nothing), fieldSourceLoc = "examples/nested_types.h:3:10"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (Just "foo") DeclPathTop, structSizeof = 8, structAlignment = 4, structFields = [StructField {fieldName = "i", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/nested_types.h:2:9"},StructField {fieldName = "c", fieldOffset = 32, fieldType = TypePrim (PrimChar Nothing), fieldSourceLoc = "examples/nested_types.h:3:10"}], structSourceLoc = "examples/nested_types.h:1:8"})})
DeclInstance (InstanceStorable (Struct {structName = "Foo", structConstr = "Foo", structFields = Field {fieldName = "foo_i", fieldType = HsPrimType HsPrimCInt, fieldOrigin = FieldOriginStructField (StructField {fieldName = "i", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/nested_types.h:2:9"})} ::: Field {fieldName = "foo_c", fieldType = HsPrimType HsPrimCChar, fieldOrigin = FieldOriginStructField (StructField {fieldName = "c", fieldOffset = 32, fieldType = TypePrim (PrimChar Nothing), fieldSourceLoc = "examples/nested_types.h:3:10"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (Just "foo") DeclPathTop, structSizeof = 8, structAlignment = 4, structFields = [StructField {fieldName = "i", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/nested_types.h:2:9"},StructField {fieldName = "c", fieldOffset = 32, fieldType = TypePrim (PrimChar Nothing), fieldSourceLoc = "examples/nested_types.h:3:10"}], structSourceLoc = "examples/nested_types.h:1:8"})}) (StorableInstance {storableSizeOf = 8, storableAlignment = 4, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "Foo", structConstr = "Foo", structFields = Field {fieldName = "foo_i", fieldType = HsPrimType HsPrimCInt, fieldOrigin = FieldOriginStructField (StructField {fieldName = "i", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/nested_types.h:2:9"})} ::: Field {fieldName = "foo_c", fieldType = HsPrimType HsPrimCChar, fieldOrigin = FieldOriginStructField (StructField {fieldName = "c", fieldOffset = 32, fieldType = TypePrim (PrimChar Nothing), fieldSourceLoc = "examples/nested_types.h:3:10"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (Just "foo") DeclPathTop, structSizeof = 8, structAlignment = 4, structFields = [StructField {fieldName = "i", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/nested_types.h:2:9"},StructField {fieldName = "c", fieldOffset = 32, fieldType = TypePrim (PrimChar Nothing), fieldSourceLoc = "examples/nested_types.h:3:10"}], structSourceLoc = "examples/nested_types.h:1:8"})})) [PeekByteOff 0 0,PeekByteOff 0 4]), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "Foo", structConstr = "Foo", structFields = Field {fieldName = "foo_i", fieldType = HsPrimType HsPrimCInt, fieldOrigin = FieldOriginStructField (StructField {fieldName = "i", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/nested_types.h:2:9"})} ::: Field {fieldName = "foo_c", fieldType = HsPrimType HsPrimCChar, fieldOrigin = FieldOriginStructField (StructField {fieldName = "c", fieldOffset = 32, fieldType = TypePrim (PrimChar Nothing), fieldSourceLoc = "examples/nested_types.h:3:10"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (Just "foo") DeclPathTop, structSizeof = 8, structAlignment = 4, structFields = [StructField {fieldName = "i", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/nested_types.h:2:9"},StructField {fieldName = "c", fieldOffset = 32, fieldType = TypePrim (PrimChar Nothing), fieldSourceLoc = "examples/nested_types.h:3:10"}], structSourceLoc = "examples/nested_types.h:1:8"})}) 2 (Seq [PokeByteOff 3 0 0,PokeByteOff 3 4 1])))}))
DeclData (Struct {structName = "Bar", structConstr = "Bar", structFields = Field {fieldName = "bar_foo1", fieldType = HsTypRef "Foo", fieldOrigin = FieldOriginStructField (StructField {fieldName = "foo1", fieldOffset = 0, fieldType = TypeStruct (DeclPathStruct (Just "foo") DeclPathTop), fieldSourceLoc = "examples/nested_types.h:7:16"})} ::: Field {fieldName = "bar_foo2", fieldType = HsTypRef "Foo", fieldOrigin = FieldOriginStructField (StructField {fieldName = "foo2", fieldOffset = 64, fieldType = TypeStruct (DeclPathStruct (Just "foo") DeclPathTop), fieldSourceLoc = "examples/nested_types.h:8:16"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (Just "bar") DeclPathTop, structSizeof = 16, structAlignment = 4, structFields = [StructField {fieldName = "foo1", fieldOffset = 0, fieldType = TypeStruct (DeclPathStruct (Just "foo") DeclPathTop), fieldSourceLoc = "examples/nested_types.h:7:16"},StructField {fieldName = "foo2", fieldOffset = 64, fieldType = TypeStruct (DeclPathStruct (Just "foo") DeclPathTop), fieldSourceLoc = "examples/nested_types.h:8:16"}], structSourceLoc = "examples/nested_types.h:6:8"})})
DeclInstance (InstanceStorable (Struct {structName = "Bar", structConstr = "Bar", structFields = Field {fieldName = "bar_foo1", fieldType = HsTypRef "Foo", fieldOrigin = FieldOriginStructField (StructField {fieldName = "foo1", fieldOffset = 0, fieldType = TypeStruct (DeclPathStruct (Just "foo") DeclPathTop), fieldSourceLoc = "examples/nested_types.h:7:16"})} ::: Field {fieldName = "bar_foo2", fieldType = HsTypRef "Foo", fieldOrigin = FieldOriginStructField (StructField {fieldName = "foo2", fieldOffset = 64, fieldType = TypeStruct (DeclPathStruct (Just "foo") DeclPathTop), fieldSourceLoc = "examples/nested_types.h:8:16"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (Just "bar") DeclPathTop, structSizeof = 16, structAlignment = 4, structFields = [StructField {fieldName = "foo1", fieldOffset = 0, fieldType = TypeStruct (DeclPathStruct (Just "foo") DeclPathTop), fieldSourceLoc = "examples/nested_types.h:7:16"},StructField {fieldName = "foo2", fieldOffset = 64, fieldType = TypeStruct (DeclPathStruct (Just "foo") DeclPathTop), fieldSourceLoc = "examples/nested_types.h:8:16"}], structSourceLoc = "examples/nested_types.h:6:8"})}) (StorableInstance {storableSizeOf = 16, storableAlignment = 4, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "Bar", structConstr = "Bar", structFields = Field {fieldName = "bar_foo1", fieldType = HsTypRef "Foo", fieldOrigin = FieldOriginStructField (StructField {fieldName = "foo1", fieldOffset = 0, fieldType = TypeStruct (DeclPathStruct (Just "foo") DeclPathTop), fieldSourceLoc = "examples/nested_types.h:7:16"})} ::: Field {fieldName = "bar_foo2", fieldType = HsTypRef "Foo", fieldOrigin = FieldOriginStructField (StructField {fieldName = "foo2", fieldOffset = 64, fieldType = TypeStruct (DeclPathStruct (Just "foo") DeclPathTop), fieldSourceLoc = "examples/nested_types.h:8:16"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (Just "bar") DeclPathTop, structSizeof = 16, structAlignment = 4, structFields = [StructField {fieldName = "foo1", fieldOffset = 0, fieldType = TypeStruct (DeclPathStruct (Just "foo") DeclPathTop), fieldSourceLoc = "examples/nested_types.h:7:16"},StructField {fieldName = "foo2", fieldOffset = 64, fieldType = TypeStruct (DeclPathStruct (Just "foo") DeclPathTop), fieldSourceLoc = "examples/nested_types.h:8:16"}], structSourceLoc = "examples/nested_types.h:6:8"})})) [PeekByteOff 0 0,PeekByteOff 0 8]), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "Bar", structConstr = "Bar", structFields = Field {fieldName = "bar_foo1", fieldType = HsTypRef "Foo", fieldOrigin = FieldOriginStructField (StructField {fieldName = "foo1", fieldOffset = 0, fieldType = TypeStruct (DeclPathStruct (Just "foo") DeclPathTop), fieldSourceLoc = "examples/nested_types.h:7:16"})} ::: Field {fieldName = "bar_foo2", fieldType = HsTypRef "Foo", fieldOrigin = FieldOriginStructField (StructField {fieldName = "foo2", fieldOffset = 64, fieldType = TypeStruct (DeclPathStruct (Just "foo") DeclPathTop), fieldSourceLoc = "examples/nested_types.h:8:16"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (Just "bar") DeclPathTop, structSizeof = 16, structAlignment = 4, structFields = [StructField {fieldName = "foo1", fieldOffset = 0, fieldType = TypeStruct (DeclPathStruct (Just "foo") DeclPathTop), fieldSourceLoc = "examples/nested_types.h:7:16"},StructField {fieldName = "foo2", fieldOffset = 64, fieldType = TypeStruct (DeclPathStruct (Just "foo") DeclPathTop), fieldSourceLoc = "examples/nested_types.h:8:16"}], structSourceLoc = "examples/nested_types.h:6:8"})}) 2 (Seq [PokeByteOff 3 0 0,PokeByteOff 3 8 1])))}))
